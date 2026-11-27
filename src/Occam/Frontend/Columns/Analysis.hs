{- | Per-unit intermediate column geometry, whole-row star projection, and
unknown-source recovery from 'StatementUnit' and source bytes.
-}
module Occam.Frontend.Columns.Analysis (
    PreParsedUnit (..),
    CreateColumns (..),
    InsertList (..),
    InsertShape (..),
    InsertColumns (..),
    ColumnUnit (..),
    emptyColumnUnit,
    collectReads,
    analyzeColumnUnit,
    analyzeColumnUnitWith,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isAsciiUpper, toLower)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Occam.Core qualified as Core
import Occam.Facts (
    ParserEdit (..),
    ParserEditCategory (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Frontend.AST qualified as AST
import Occam.Frontend.Columns.Edits (
    buildTokenTable,
    columnDefSpans,
    commaDeleteSpans,
    insertListItemSpans,
    isInsertTainted,
    valuesItemSpans,
 )
import Occam.Frontend.Context (
    IdentifierClass (..),
    IdentifierRules,
    canonicalIdentWith,
    identifierRulesFor,
    namePartsWith,
    relationFromParts,
    unquoteNamePart,
    unwrapExplainAnalyze,
 )
import Occam.Frontend.Lexer (
    SpannedToken,
    Token (..),
    WordToken (..),
    asciiEqCI,
    isTriviaToken,
    lexSQLStrict,
 )
import Occam.Frontend.Parser (parseUnit)
import Occam.Frontend.Span (ByteSpan (..), Located (..), sliceSpan)
import Occam.Frontend.Splitter (SplitUnit (..), splitSQL)
import Occam.Parser.Dialect (
    DialectProfile (..),
    clickhouseProfile,
    duckdbProfile,
    mariadbProfile,
    mysqlProfile,
    postgresqlProfile,
    sqliteProfile,
 )
import Occam.Parser.Types (SQLDialect, defaultServerConfig)

-- | One @CREATE TABLE@ column list in definition order with optional deletion spans.
newtype CreateColumns = CreateColumns
    { createColumns :: [(Core.Ident, Maybe (Int, Int))]
    }
    deriving (Eq, Show)

-- | Shape of an @INSERT@ statement's data source.
data InsertShape
    = ShapePlainValues
    | ShapeSelectSource
    | ShapeNeutral
    | ShapeExotic
    deriving (Eq, Show)

data InsertList = InsertList
    { insertListWidth :: !Int
    , insertListColumns :: !(Map Core.Ident (Maybe (Int, Maybe (Int, Int))))
    -- ^ A present key with no position is an ambiguous duplicate name.
    }
    deriving (Eq, Show)

-- | One @INSERT@ statement's column geometry.
data InsertColumns = InsertColumns
    { insertTarget :: !(Maybe Core.NameRef)
    , insertExplicit :: !(Maybe InsertList)
    , insertRows :: ![Vector (Maybe (Int, Int))]
    , insertRowWidth :: !(Maybe Int)
    , insertShape :: !InsertShape
    }
    deriving (Eq, Show)

-- | Per-unit intermediate column data consumed by 'Occam.Frontend.Columns.Assemble'.
data ColumnUnit = ColumnUnit
    { cuCreates :: ![CreateColumns]
    , cuInserts :: ![InsertColumns]
    , cuStars :: ![Core.NameRef]
    , cuUnknownSource :: !Bool
    }
    deriving (Eq, Show)

-- | Empty 'ColumnUnit'.
emptyColumnUnit :: ColumnUnit
emptyColumnUnit = ColumnUnit [] [] [] False

-- | Every read of a base table in one unit's semantic ops.
collectReads :: Core.SemanticFacts -> [Core.ReadFact]
collectReads facts = concatMap opReads (Core.factOps facts)
    where
        opReads = \case
            Core.ReadOp r -> [r]
            Core.WriteDataOp w -> Core.dataWriteReads w
            Core.DefineTableOp t -> Core.tableSources t
            Core.DefineViewOp v -> Core.viewSources v
            Core.DefineTriggerOp tr ->
                Core.triggerWhenReads tr ++ case Core.triggerBody tr of
                    Core.TriggerInline effects -> [r | Core.TriggerRead r <- effects]
                    _ -> []
            _ -> []

-- | Pre-parsed AST and token stream supplied by the pipeline to avoid re-parsing.
data PreParsedUnit = PreParsedUnit
    { ppProfile :: !DialectProfile
    , ppRules :: !IdentifierRules
    , ppTokens :: !(Vector SpannedToken)
    , ppAst :: !AST.Statement
    }

-- | Reconstruct 'ColumnUnit' for one parsed 'StatementUnit'.
analyzeColumnUnit :: ByteString -> StatementUnit -> ColumnUnit
analyzeColumnUnit = analyzeColumnUnitWith Nothing

-- | Reconstruct 'ColumnUnit', reusing a pre-parsed AST/token stream when available.
analyzeColumnUnitWith :: Maybe PreParsedUnit -> ByteString -> StatementUnit -> ColumnUnit
analyzeColumnUnitWith mPreParsed source unit = case unitFacts unit of
    Nothing -> emptyColumnUnit
    Just facts ->
        let sem = factSemantics facts
            edits = factEdits facts
            raw = Core.stmtRaw (unitStatement unit)
            Core.Span uStart _ = Core.stmtSpan (unitStatement unit)
            readsList = collectReads sem
            tableDefs = [td | Core.DefineTableOp td <- Core.factOps sem]
            createDefEdits =
                [ (parserEditStart e, parserEditEnd e)
                | e <- edits
                , parserEditCategory e == EditColumn
                , parserEditLabel e == "remove-column_create_def"
                ]
            needsCreateParse =
                any
                    ( \td ->
                        let nCols = length (Core.tableColumns td)
                         in nCols > 0 && not (null createDefEdits) && length createDefEdits /= nCols
                    )
                    tableDefs
            hasInsertWrite = any isInsertWriteOp (Core.factOps sem)
            hasUnknownDml = any ((== Core.UnknownDMLBarrier) . Core.barrierReason) (Core.factBarriers sem)
            needsInsert = hasInsertWrite || hasUnknownDml
            needsStar =
                any (null . Core.readColumns) readsList
                    && (BS.elem 42 raw || hasAsciiWord "TABLE" raw)
            parsed =
                if needsCreateParse || needsInsert || needsStar
                    then case mPreParsed of
                        Just pp -> Just (ppProfile pp, ppRules pp, ppTokens pp, ppAst pp, source, 0)
                        Nothing ->
                            fmap
                                ( \(prof, toks, stmt) ->
                                    (prof, identifierRulesFor (dialectId prof) defaultServerConfig, toks, stmt, raw, uStart)
                                )
                                (parseBestDialect raw)
                    else Nothing
            creates = recoverCreates source tableDefs createDefEdits parsed
            inserts =
                if needsInsert
                    then recoverInserts sem edits raw parsed
                    else []
            (stars, unkSrc) =
                if needsStar
                    then recoverStars readsList parsed
                    else ([], False)
         in ColumnUnit
                { cuCreates = creates
                , cuInserts = inserts
                , cuStars = stars
                , cuUnknownSource = unkSrc
                }

isInsertWriteOp :: Core.CatalogOp -> Bool
isInsertWriteOp (Core.WriteDataOp dw) = case Core.dataWriteKind dw of
    Core.InsertWrite _ -> True
    _ -> False
isInsertWriteOp _ = False

-- ---------------------------------------------------------------------------
-- Multi-dialect statement parsing for unit geometry
-- ---------------------------------------------------------------------------

parseBestDialect :: ByteString -> Maybe (DialectProfile, Vector SpannedToken, AST.Statement)
parseBestDialect raw = go (candidateProfiles raw)
    where
        go [] = Nothing
        go (prof : rest) = case splitSQL prof raw of
            Right [su]
                | isNothing (splitUnitDelimiterDirective su)
                , Right (L _ stmt) <- parseUnit prof su ->
                    Just (prof, splitUnitTokens su, stmt)
            _ -> go rest

candidateProfiles :: ByteString -> [DialectProfile]
candidateProfiles raw
    | BS.elem 96 raw =
        [mysqlProfile, mariadbProfile, sqliteProfile, duckdbProfile, clickhouseProfile, postgresqlProfile]
    | BS.elem 91 raw =
        [sqliteProfile, postgresqlProfile, duckdbProfile, clickhouseProfile, mysqlProfile, mariadbProfile]
    | hasAsciiWord "ROW" raw =
        [mysqlProfile, mariadbProfile, sqliteProfile, postgresqlProfile, duckdbProfile, clickhouseProfile]
    | hasAsciiWord "RETURNING" raw =
        [postgresqlProfile, sqliteProfile, mariadbProfile, duckdbProfile, mysqlProfile, clickhouseProfile]
    | hasAsciiWord "CONFLICT" raw =
        [postgresqlProfile, sqliteProfile, duckdbProfile, mysqlProfile, mariadbProfile, clickhouseProfile]
    | otherwise =
        [sqliteProfile, postgresqlProfile, mysqlProfile, mariadbProfile, duckdbProfile, clickhouseProfile]

hasAsciiWord :: ByteString -> ByteString -> Bool
hasAsciiWord needle haystack = go 0
    where
        !nLen = BS.length needle
        !hLen = BS.length haystack
        go !i
            | i + nLen > hLen = False
            | asciiEqCI needle (BS.take nLen (BS.drop i haystack))
            , i == 0 || not (isIdentByte (BS.index haystack (i - 1)))
            , i + nLen == hLen || not (isIdentByte (BS.index haystack (i + nLen))) =
                True
            | otherwise = go (i + 1)
        isIdentByte w =
            let c = toEnum (fromIntegral w)
             in isAlphaNum c || c == '_'

-- ---------------------------------------------------------------------------
-- CREATE TABLE recovery
-- ---------------------------------------------------------------------------

type ResolvedParse =
    (DialectProfile, IdentifierRules, Vector SpannedToken, AST.Statement, ByteString, Int)

recoverCreates ::
    ByteString ->
    [Core.TableDef] ->
    [(Int, Int)] ->
    Maybe ResolvedParse ->
    [CreateColumns]
recoverCreates source tableDefs createDefEdits parsed =
    mapMaybe buildCreate tableDefs
    where
        astCreateCols = case parsed of
            Just (_, _, tokens, rawStmt, astSrc, spanShift) -> case unwrapExplainAnalyze rawStmt of
                AST.StatementCreate (L _ cs) -> case locValue (AST.createObject cs) of
                    AST.CreateTableObject ct ->
                        let tokTable = buildTokenTable tokens
                         in Just (map (fmap (shiftPair spanShift)) (columnDefSpans astSrc 0 tokTable ct))
                    _ -> Nothing
                _ -> Nothing
            Nothing -> Nothing

        buildCreate td
            | null cols = Nothing
            | length createDefEdits == length cols =
                Just (CreateColumns (zipWith (\c sp -> (Core.columnName c, Just sp)) cols createDefEdits))
            | null createDefEdits =
                Just (CreateColumns [(Core.columnName c, Nothing) | c <- cols])
            | Just astSpans <- astCreateCols
            , length astSpans == length cols =
                Just (CreateColumns (zipWith (\c sp -> (Core.columnName c, sp)) cols astSpans))
            | otherwise =
                Just (CreateColumns (matchEditsToCols source cols createDefEdits))
            where
                cols = Core.tableColumns td

matchEditsToCols ::
    ByteString -> [Core.ColumnDef] -> [(Int, Int)] -> [(Core.Ident, Maybe (Int, Int))]
matchEditsToCols source = go
    where
        go [] _ = []
        go (c : cs) [] = (Core.columnName c, Nothing) : go cs []
        go (c : cs) (sp@(s, e) : rest)
            | editMatchesCol c s e = (Core.columnName c, Just sp) : go cs rest
            | otherwise = (Core.columnName c, Nothing) : go cs (sp : rest)

        editMatchesCol col s e = case sliceSpan (ByteSpan s e) source of
            Nothing -> False
            Just bs -> case lexSQLStrict sqliteProfile bs of
                Left _ -> False
                Right toks ->
                    let sig = dropLeadingCommaAndTrivia (V.toList toks)
                     in case sig of
                            (L _ (TokWord wt) : _) ->
                                let rawName = unquoteNamePart (AST.NamePart (wordRaw wt) (isJust (wordQuote wt)))
                                 in asciiEqText (Core.identText (Core.columnName col)) rawName
                            _ -> False

        dropLeadingCommaAndTrivia = \case
            (L _ TokComma : rest) -> dropLeadingCommaAndTrivia rest
            (L _ tok : rest) | isTriviaToken tok -> dropLeadingCommaAndTrivia rest
            xs -> xs

-- ---------------------------------------------------------------------------
-- INSERT recovery
-- ---------------------------------------------------------------------------

recoverInserts ::
    Core.SemanticFacts ->
    [ParserEdit] ->
    ByteString ->
    Maybe ResolvedParse ->
    [InsertColumns]
recoverInserts sem edits raw parsed =
    case parsed of
        Just (profile, rules, tokens, rawStmt, _, spanShift) -> case unwrapExplainAnalyze rawStmt of
            AST.StatementInsert (L _ ins) -> [fromAstInsert profile rules tokens spanShift ins]
            _ -> []
        Nothing
            | startsAsInsertOrReplace raw -> [fallbackExoticInsert]
            | otherwise -> []
    where
        insertWrites =
            [ dw
            | Core.WriteDataOp dw <- Core.factOps sem
            , case Core.dataWriteKind dw of
                Core.InsertWrite _ -> True
                _ -> False
            ]
        unmodeledInsertBarrier =
            any
                ( \b ->
                    Core.barrierReason b == Core.UnknownDMLBarrier
                        && Core.barrierDetail b
                            == Just "unmodeled overwrite, partition, or multi-table INSERT"
                )
                (Core.factBarriers sem)
        insertItemEdits =
            [ (parserEditStart e, parserEditEnd e)
            | e <- edits
            , parserEditCategory e == EditColumn
            , parserEditLabel e == "remove-column_insert_item"
            ]
        valuesItemEdits =
            [ (parserEditStart e, parserEditEnd e)
            | e <- edits
            , parserEditCategory e == EditColumn
            , parserEditLabel e == "remove-column_values_item"
            ]

        fromAstInsert profile rules tokens spanShift ins =
            let tokTable = buildTokenTable tokens
                target = case insertWrites of
                    (dw : _) -> Just (Core.dataWriteTarget dw)
                    []
                        | unmodeledInsertBarrier ->
                            fmap (relationFromParts (dialectId profile))
                                . namePartsWith rules IdentRelation
                                . locValue
                                $ AST.insertTable ins
                        | otherwise -> Nothing
                dwCols = maybe [] Core.dataWriteColumns (listToMaybe insertWrites)
                astCols = AST.insertColumns ins
                mExplicitSpans = insertListItemSpans 0 tokTable astCols
                explicitDels = case mExplicitSpans of
                    Just spans -> map (fmap (shiftPair spanShift)) (commaDeleteSpans tokTable spans)
                    Nothing -> []
                tainted =
                    isInsertTainted ins
                        || unmodeledInsertBarrier
                        || (not (null astCols) && length explicitDels /= length astCols)
                explicit =
                    if null astCols
                        then Nothing
                        else
                            let idents = resolveInsertColIdents rules dwCols tainted astCols
                                delSpans
                                    | length astCols == 1 = [Nothing]
                                    | length insertItemEdits == length astCols = map Just insertItemEdits
                                    | length explicitDels == length astCols = explicitDels
                                    | otherwise = replicate (length astCols) Nothing
                                positions =
                                    Map.fromListWith
                                        (\_ _ -> Nothing)
                                        [ (name, Just (pos, geometry))
                                        | (pos, name, geometry) <- zip3 [0 ..] idents delSpans
                                        ]
                             in Just (InsertList (length idents) positions)
                (shape, rows)
                    | tainted = (ShapeExotic, [])
                    | otherwise = case AST.insertSource ins of
                        AST.InsertDefaultValues -> (ShapeNeutral, [])
                        AST.InsertQuery _ -> (ShapeSelectSource, [])
                        AST.InsertValues astRows ->
                            (ShapePlainValues, map V.fromList (recoverRows spanShift tokTable astRows))
                width = case rows of
                    first : rest
                        | let n = V.length first
                        , all ((== n) . V.length) rest ->
                            Just n
                    _ -> Nothing
             in InsertColumns
                    { insertTarget = target
                    , insertExplicit = explicit
                    , insertRows = rows
                    , insertRowWidth = width
                    , insertShape = shape
                    }

        recoverRows spanShift tokTable astRows =
            let expectedFromEdits = sum [length r | r <- astRows, length r >= 2]
             in if not (null astRows)
                    && not (any null astRows)
                    && expectedFromEdits == length valuesItemEdits
                    then splitRowEdits astRows valuesItemEdits
                    else
                        [ maybe
                            []
                            (map (fmap (shiftPair spanShift)) . commaDeleteSpans tokTable)
                            (valuesItemSpans 0 tokTable row)
                        | row <- astRows
                        ]

        splitRowEdits [] _ = []
        splitRowEdits (r : rs) eds
            | length r == 1 = [Nothing] : splitRowEdits rs eds
            | otherwise =
                let (here, rest) = splitAt (length r) eds
                 in map Just here : splitRowEdits rs rest

        resolveInsertColIdents rules dwCols tainted astCols
            | not tainted && length dwCols == length astCols = dwCols
            | otherwise = map (foldColPart rules dwCols) astCols

        foldColPart rules dwCols (L _ part) =
            let fallback = canonicalIdentWith rules IdentColumn part
             in case filter (asciiEqText (Core.identText fallback) . Core.identText) dwCols of
                    (exact : _) -> exact
                    [] -> fallback

        fallbackExoticInsert =
            InsertColumns
                { insertTarget = fmap Core.dataWriteTarget (listToMaybe insertWrites)
                , insertExplicit = Nothing
                , insertRows = []
                , insertRowWidth = Nothing
                , insertShape = ShapeExotic
                }

startsAsInsertOrReplace :: ByteString -> Bool
startsAsInsertOrReplace raw = case lexSQLStrict sqliteProfile raw of
    Left _ -> False
    Right toks -> go 0 (dropTrivia (V.toList toks))
    where
        dropTrivia = filter (not . isTriviaToken . locValue)
        go :: Int -> [SpannedToken] -> Bool
        go !depth = \case
            [] -> False
            (L _ TokLParen : rest) -> go (depth + 1) rest
            (L _ TokRParen : rest) -> go (max 0 (depth - 1)) rest
            (L _ (TokWord wt) : rest)
                | depth == 0 && isNothing (wordQuote wt) ->
                    let w = wordRaw wt
                     in asciiEqCI "INSERT" w || asciiEqCI "REPLACE" w || go depth rest
            (_ : rest) -> go depth rest

-- ---------------------------------------------------------------------------
-- Star projection and unknown-source recovery
-- ---------------------------------------------------------------------------

recoverStars ::
    [Core.ReadFact] ->
    Maybe ResolvedParse ->
    ([Core.NameRef], Bool)
recoverStars readsList parsed = case parsed of
    Nothing -> ([], False)
    Just (profile, rules, _tokens, rawStmt, _, _) ->
        let stmt = unwrapExplainAnalyze rawStmt
            (starredRefs, unkSrc) = collectStmtStars (dialectId profile) rules stmt
            emptyColReads =
                [ Core.readSource r
                | r <- readsList
                , null (Core.readColumns r)
                ]
            matched =
                nub
                    [ nr
                    | nr <- emptyColReads
                    , any (starMatchesRead nr) starredRefs
                    ]
         in (matched, unkSrc)
    where
        starMatchesRead nr starRef =
            Core.nameRefTemp nr == Core.nameRefTemp starRef
                && nameAsciiEq (Core.nameRefName nr) (Core.nameRefName starRef)

data FromEntry
    = BaseFrom !Core.NameRef !(Maybe Text)
    | OpaqueAlias !Text

type StarAcc = ([Core.NameRef], Bool)

combineStar :: [StarAcc] -> StarAcc
combineStar xs = (concatMap fst xs, any snd xs)

collectStmtStars :: SQLDialect -> IdentifierRules -> AST.Statement -> StarAcc
collectStmtStars dialect rules = walkStmt Set.empty
    where
        foldRel = canonicalIdentWith rules IdentRelation

        walkStmt ctes = \case
            AST.StatementQuery (L _ q) -> walkQuery ctes q
            AST.StatementInsert (L _ ins) ->
                let ctes' = maybe ctes ((`withCteNames` ctes) . locValue) (AST.insertWith ins)
                    withAcc = maybe ([], False) (walkWith ctes . locValue) (AST.insertWith ins)
                    srcAcc = case AST.insertSource ins of
                        AST.InsertQuery (L _ q) -> walkQuery ctes' q
                        AST.InsertValues rows -> combineStar (concatMap (map (walkExpr ctes' . locValue)) rows)
                        AST.InsertDefaultValues -> ([], False)
                    retAcc =
                        combineStar [walkExpr ctes' (locValue (AST.selectItemExpr it)) | L _ it <- AST.insertReturning ins]
                 in combineStar [withAcc, srcAcc, retAcc]
            AST.StatementUpdate (L _ upd) ->
                let ctes' = maybe ctes ((`withCteNames` ctes) . locValue) (AST.updateWith upd)
                    withAcc = maybe ([], False) (walkWith ctes . locValue) (AST.updateWith upd)
                    tgtAcc = walkFromSubqueries ctes' (AST.updateTargets upd)
                    fromAcc = maybe ([], False) (walkFromSubqueries ctes') (AST.updateFrom upd)
                    assnAcc = combineStar [walkExpr ctes' (locValue (AST.assignValue a)) | L _ a <- AST.updateAssignments upd]
                    whereAcc = maybe ([], False) (walkExpr ctes' . locValue) (AST.updateWhere upd)
                    retAcc =
                        combineStar [walkExpr ctes' (locValue (AST.selectItemExpr it)) | L _ it <- AST.updateReturning upd]
                 in combineStar [withAcc, tgtAcc, fromAcc, assnAcc, whereAcc, retAcc]
            AST.StatementDelete (L _ del) ->
                let ctes' = maybe ctes ((`withCteNames` ctes) . locValue) (AST.deleteWith del)
                    withAcc = maybe ([], False) (walkWith ctes . locValue) (AST.deleteWith del)
                    fromAcc = maybe ([], False) (walkFromSubqueries ctes') (AST.deleteFromTables del)
                    usingAcc = maybe ([], False) (walkFromSubqueries ctes') (AST.deleteUsing del)
                    whereAcc = maybe ([], False) (walkExpr ctes' . locValue) (AST.deleteWhere del)
                    retAcc =
                        combineStar [walkExpr ctes' (locValue (AST.selectItemExpr it)) | L _ it <- AST.deleteReturning del]
                 in combineStar [withAcc, fromAcc, usingAcc, whereAcc, retAcc]
            AST.StatementMerge (L _ mrg) ->
                let ctes' = maybe ctes ((`withCteNames` ctes) . locValue) (AST.mergeWith mrg)
                    withAcc = maybe ([], False) (walkWith ctes . locValue) (AST.mergeWith mrg)
                    srcAcc = walkTableRefSubqueries ctes' (locValue (AST.mergeSource mrg))
                    onAcc = walkExpr ctes' (locValue (AST.mergeOn mrg))
                 in combineStar [withAcc, srcAcc, onAcc]
            AST.StatementCreate (L _ cr) -> case locValue (AST.createObject cr) of
                AST.CreateTableObject ct ->
                    maybe ([], False) (walkQuery ctes . locValue) (AST.createTableAs ct)
                AST.CreateViewObject cv -> case AST.createViewBody cv of
                    AST.ViewBodyQuery (L _ q) -> walkQuery ctes q
                    AST.ViewBodyOpaque _ -> ([], False)
                AST.CreateMacroObject cm -> case AST.createMacroBody cm of
                    Left (L _ q) -> walkQuery ctes q
                    Right (L _ e) -> walkExpr ctes e
                AST.CreateRuleObject rule ->
                    let wAcc = maybe ([], False) (walkExpr ctes . locValue) (AST.createRuleWhere rule)
                        aAcc = maybe ([], False) (walkStmt ctes . locValue) (AST.createRuleAction rule)
                     in combineStar [wAcc, aAcc]
                AST.CreatePolicyObject pol ->
                    combineStar
                        [ maybe ([], False) (walkExpr ctes . locValue) (AST.createPolicyUsing pol)
                        , maybe ([], False) (walkExpr ctes . locValue) (AST.createPolicyWithCheck pol)
                        ]
                AST.CreateTriggerObject tr ->
                    maybe ([], False) (walkExpr ctes . locValue) (AST.createTriggerWhen tr)
                _ -> ([], False)
            AST.StatementAlter (L _ alt) ->
                combineStar [walkAlterAction ctes a | L _ a <- AST.alterActions alt]
            AST.StatementCopy (L _ cp) -> case AST.copySource cp of
                AST.CopySourceQuery (L _ q) -> walkQuery ctes q
                _ -> ([], False)
            AST.StatementExplain (L _ ex) -> walkStmt ctes (locValue (AST.explainStatement ex))
            _ -> ([], False)

        walkAlterAction ctes = \case
            AST.AlterViewAsQuery (L _ q) -> walkQuery ctes q
            _ -> ([], False)

        cteIdent (L _ part) = asciiLowerText (Core.identText (foldRel part))

        withCteNames wc =
            Set.union (Set.fromList [cteIdent (AST.cteName cte) | L _ cte <- AST.withCtes wc])

        walkWith ctes wc
            | AST.withRecursive wc =
                let ctes' = withCteNames wc ctes
                 in combineStar [walkCteBody ctes' (locValue (AST.cteBody cte)) | L _ cte <- AST.withCtes wc]
            | otherwise = go ctes (AST.withCtes wc)
            where
                go _ [] = ([], False)
                go accCtes (L _ cte : rest) =
                    let here = walkCteBody accCtes (locValue (AST.cteBody cte))
                        accCtes' = Set.insert (cteIdent (AST.cteName cte)) accCtes
                     in combineStar [here, go accCtes' rest]

        walkCteBody ctes = \case
            AST.CteQuery (L _ q) -> walkQuery ctes q
            AST.CteStatement _ -> ([], False)

        walkQuery ctes q =
            let ctes' = maybe ctes ((`withCteNames` ctes) . locValue) (AST.queryWith q)
                withAcc = maybe ([], False) (walkWith ctes . locValue) (AST.queryWith q)
                bodyAcc = walkQueryBody ctes' (locValue (AST.queryBody q))
                orderAcc = combineStar [walkExpr ctes' (locValue (AST.orderExpr oi)) | L _ oi <- AST.queryOrderBy q]
                limAcc = maybe ([], False) (walkLimit ctes' . locValue) (AST.queryLimit q)
                offAcc = maybe ([], False) (walkExpr ctes' . locValue) (AST.queryOffset q)
             in combineStar [withAcc, bodyAcc, orderAcc, limAcc, offAcc]

        walkLimit ctes lim =
            combineStar
                [ walkExpr ctes (locValue (AST.limitFirst lim))
                , maybe ([], False) (walkExpr ctes . locValue) (AST.limitSecond lim)
                ]

        walkQueryBody ctes = \case
            AST.QueryBodySelect core -> walkSelect ctes core
            AST.QueryBodyFrom core -> walkSelect ctes core
            AST.QueryBodySet _ _ (L _ l) (L _ r) ->
                combineStar [walkQueryBody ctes l, walkQueryBody ctes r]
            AST.QueryBodyParen (L _ q) -> walkQuery ctes q
            AST.QueryBodyValues rows ->
                combineStar (concatMap (map (walkExpr ctes . locValue)) rows)
            AST.QueryBodyTable (L _ name) ->
                case AST.nameParts name of
                    [part] | Set.member (cteIdent part) ctes -> ([], False)
                    _ -> case namePartsWith rules IdentRelation name of
                        Just parts -> ([relationFromParts dialect parts], False)
                        Nothing -> ([], True)

        walkSelect ctes core =
            let (entries, subAcc) = maybe ([], ([], False)) (collectFrom ctes) (AST.selectFrom core)
                projAcc = combineStar [walkSelectItem ctes entries it | L _ it <- AST.selectItems core]
                whereAcc = maybe ([], False) (walkExpr ctes . locValue) (AST.selectWhere core)
                preAcc = maybe ([], False) (walkExpr ctes . locValue . locValue) (AST.selectPrewhere core)
                havAcc = maybe ([], False) (walkExpr ctes . locValue) (AST.selectHaving core)
             in combineStar [subAcc, projAcc, whereAcc, preAcc, havAcc]

        walkSelectItem ctes entries item = case locValue (AST.selectItemExpr item) of
            AST.ExprStar -> ([nr | BaseFrom nr _ <- entries], False)
            AST.ExprQualifiedStar (L _ qName) ->
                case namePartsWith rules IdentRelation qName of
                    Nothing -> ([], True)
                    Just [Core.Ident qTxt] ->
                        let qLow = asciiLowerText qTxt
                            aliasMatches = [e | e <- entries, entryHasAlias qLow e]
                         in if not (null aliasMatches)
                                then ([nr | BaseFrom nr _ <- aliasMatches], False)
                                else
                                    ( [ nr
                                      | BaseFrom nr Nothing <- entries
                                      , Just (Core.Ident lastTxt) <- [listToMaybe (reverse (Core.nameParts (Core.nameRefName nr)))]
                                      , asciiLowerText lastTxt == qLow
                                      ]
                                    , False
                                    )
                    Just qParts ->
                        let qNameVal = Core.Name qParts
                         in ( [ nr
                              | BaseFrom nr Nothing <- entries
                              , nameAsciiEq (Core.nameRefName nr) qNameVal
                              ]
                            , False
                            )
            AST.ExprFieldStar (L _ inner) ->
                combineStar [([], True), walkExpr ctes inner]
            other -> walkExpr ctes other

        entryHasAlias qLow = \case
            BaseFrom _ (Just a) -> a == qLow
            BaseFrom _ Nothing -> False
            OpaqueAlias a -> a == qLow

        collectFrom ctes fc =
            let res = map (collectTableRef ctes . locValue) (AST.fromItems fc)
             in (concatMap fst res, combineStar (map snd res))

        collectTableRef ctes ref =
            let (srcEntries, srcAcc) = collectTableSource ctes (AST.tableRefAlias ref) (locValue (AST.tableRefSource ref))
                joinRes = map (collectJoin ctes . locValue) (AST.tableRefJoins ref)
             in (srcEntries ++ concatMap fst joinRes, combineStar (srcAcc : map snd joinRes))

        aliasText = fmap (cteIdent . AST.aliasName . locValue)

        collectTableSource ctes mAlias = \case
            AST.TableSourceName (L _ name) ->
                case AST.nameParts name of
                    [part]
                        | Set.member (cteIdent part) ctes ->
                            let entries = maybe [] (\a -> [OpaqueAlias a]) (aliasText mAlias)
                             in (entries, ([], False))
                    _ -> case namePartsWith rules IdentRelation name of
                        Just parts ->
                            ([BaseFrom (relationFromParts dialect parts) (aliasText mAlias)], ([], False))
                        Nothing -> ([], ([], True))
            AST.TableSourceSubquery (L _ q) ->
                let entries = maybe [] (\a -> [OpaqueAlias a]) (aliasText mAlias)
                 in (entries, walkQuery ctes q)
            AST.TableSourceLateral (L _ inner) ->
                collectTableSource ctes mAlias inner
            AST.TableSourceParens fc ->
                collectFrom ctes fc
            AST.TableSourceValues rows ->
                let entries = maybe [] (\a -> [OpaqueAlias a]) (aliasText mAlias)
                 in (entries, combineStar (concatMap (map (walkExpr ctes . locValue)) rows))
            AST.TableSourceFunction (L _ fc) ->
                let entries = maybe [] (\a -> [OpaqueAlias a]) (aliasText mAlias)
                 in (entries, walkFuncArgs ctes (AST.functionArgs fc))

        collectJoin ctes jc =
            let (tEntries, tAcc) = case AST.joinTarget jc of
                    AST.JoinTargetTable (L _ ref) -> collectTableRef ctes ref
                    AST.JoinTargetArray items ->
                        ( []
                        , combineStar [walkExpr ctes (locValue (AST.arrayJoinExpr it)) | L _ it <- items]
                        )
                cAcc = case AST.joinConstraint jc of
                    Just (AST.JoinOn (L _ e)) -> walkExpr ctes e
                    _ -> ([], False)
             in (tEntries, combineStar [tAcc, cAcc])

        walkFromSubqueries ctes fc = snd (collectFrom ctes fc)
        walkTableRefSubqueries ctes ref = snd (collectTableRef ctes ref)

        walkFuncArgs ctes args = combineStar [walkArg a | L _ a <- args]
            where
                walkArg = \case
                    AST.FunctionArgExpr (L _ e) -> walkExpr ctes e
                    AST.FunctionArgNamed _ (L _ e) -> walkExpr ctes e
                    AST.FunctionArgStar -> ([], False)

        walkExpr ctes = \case
            AST.ExprUnary _ (L _ e) -> walkExpr ctes e
            AST.ExprBinary _ (L _ l) (L _ r) -> combineStar [walkExpr ctes l, walkExpr ctes r]
            AST.ExprCast _ (L _ e) _ -> walkExpr ctes e
            AST.ExprCastExpr (L _ e) (L _ t) -> combineStar [walkExpr ctes e, walkExpr ctes t]
            AST.ExprLambda _ (L _ e) -> walkExpr ctes e
            AST.ExprSubscript (L _ e) a b ->
                combineStar
                    [ walkExpr ctes e
                    , maybe ([], False) (walkExpr ctes . locValue) a
                    , maybe ([], False) (walkExpr ctes . locValue) b
                    ]
            AST.ExprField (L _ e) _ -> walkExpr ctes e
            AST.ExprFieldIndex (L _ e) _ -> walkExpr ctes e
            AST.ExprFieldStar (L _ e) -> walkExpr ctes e
            AST.ExprCall (L _ fc) ->
                combineStar
                    [ walkFuncArgs ctes (AST.functionArgs fc)
                    , maybe ([], False) (walkExpr ctes . locValue) (AST.functionFilter fc)
                    ]
            AST.ExprCase ce ->
                combineStar $
                    maybe ([], False) (walkExpr ctes . locValue) (AST.caseOperand ce)
                        : maybe ([], False) (walkExpr ctes . locValue) (AST.caseElse ce)
                        : concat [[walkExpr ctes (locValue w), walkExpr ctes (locValue t)] | (w, t) <- AST.caseWhens ce]
            AST.ExprExists (L _ q) -> walkQuery ctes q
            AST.ExprIsNull _ (L _ e) -> walkExpr ctes e
            AST.ExprIsTrue _ (L _ e) -> walkExpr ctes e
            AST.ExprIsFalse _ (L _ e) -> walkExpr ctes e
            AST.ExprIsUnknown _ (L _ e) -> walkExpr ctes e
            AST.ExprIsDistinct _ (L _ l) (L _ r) -> combineStar [walkExpr ctes l, walkExpr ctes r]
            AST.ExprBetween _ (L _ e) (L _ l) (L _ h) ->
                combineStar [walkExpr ctes e, walkExpr ctes l, walkExpr ctes h]
            AST.ExprInList _ (L _ e) xs ->
                combineStar (walkExpr ctes e : [walkExpr ctes (locValue x) | x <- xs])
            AST.ExprInSubquery _ (L _ e) (L _ q) ->
                combineStar [walkExpr ctes e, walkQuery ctes q]
            AST.ExprLike _ _ (L _ e) (L _ p) esc ->
                combineStar
                    [ walkExpr ctes e
                    , walkExpr ctes p
                    , maybe ([], False) (walkExpr ctes . locValue) esc
                    ]
            AST.ExprQuantified _ _ (L _ l) (L _ r) -> combineStar [walkExpr ctes l, walkExpr ctes r]
            AST.ExprArray xs -> combineStar [walkExpr ctes (locValue x) | x <- xs]
            AST.ExprStruct xs ->
                combineStar (concat [[walkExpr ctes (locValue a), walkExpr ctes (locValue b)] | (a, b) <- xs])
            AST.ExprTuple xs -> combineStar [walkExpr ctes (locValue x) | x <- xs]
            AST.ExprSubquery (L _ q) -> walkQuery ctes q
            AST.ExprParen (L _ e) -> walkExpr ctes e
            AST.ExprCollate (L _ e) _ -> walkExpr ctes e
            _ -> ([], False)

-- ---------------------------------------------------------------------------
-- ASCII comparison helpers
-- ---------------------------------------------------------------------------

asciiLowerText :: Text -> Text
asciiLowerText = T.map (\c -> if isAsciiUpper c then toLower c else c)

shiftPair :: Int -> (Int, Int) -> (Int, Int)
shiftPair !offset (!s, !e) = (offset + s, offset + e)

asciiEqText :: Text -> Text -> Bool
asciiEqText a b = asciiLowerText a == asciiLowerText b

nameAsciiEq :: Core.Name -> Core.Name -> Bool
nameAsciiEq (Core.Name as) (Core.Name bs) =
    length as == length bs && and (zipWith (\(Core.Ident x) (Core.Ident y) -> asciiEqText x y) as bs)
