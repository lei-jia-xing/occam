-- | Pure-Haskell parsing pipeline producing 'ParsedSQL' from 'ParseOptions' and SQL bytes.
module Occam.Frontend.Pipeline (
    parseFrontend,
    sessionInit,
    statementCannotProduceResult,
    isRoutineDDL,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Occam.Core qualified as Core
import Occam.Facts (
    ParserEdit (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Frontend.AST qualified as AST
import Occam.Frontend.Columns (PreParsedUnit (..), assembleColumnsWith, columnEdits)
import Occam.Frontend.Context (
    AnalysisContext (..),
    IdentifierClass (..),
    IdentifierRules,
    foldIdentText,
    identifierRulesFor,
    literalIdentifierPart,
    makeAnalysisContext,
    unwrapExplainAnalyze,
 )
import Occam.Frontend.Edits (statementEdits)
import Occam.Frontend.Lexer (LexError (..), SpannedToken)
import Occam.Frontend.Parser (ParseError (..), parseUnit)
import Occam.Frontend.Semantics (emptySemanticFacts, statementSemantics)
import Occam.Frontend.Span (
    ByteSpan (..),
    Located (..),
    isUtf8Boundary,
    unsafeSliceSpan,
 )
import Occam.Frontend.Splitter (SplitUnit (..), splitSQL)
import Occam.Parser.Dialect (
    DialectProfile (..),
    SemanticsProfile (..),
    dialectProfile,
 )
import Occam.Parser.Types (
    ParseOptions (..),
    ParsedSQL (..),
    ParserError (..),
    ParserErrorKind (..),
    ServerConfig (..),
 )
import Occam.Parser.Validate (validateColumns)

-- | Intermediate parsed unit held until the script-wide @pureTableEnabled@ flag is known.
data PendingUnit = PendingUnit
    { pendingStart :: !Int
    , pendingEnd :: !Int
    , pendingTokens :: !(Vector SpannedToken)
    , pendingResult :: !(Either Text AST.Statement)
    }

{- | Parse @sql@ under @options@ using the pure-Haskell lexer, splitter, parser,
and analysis passes.

* Rejects invalid UTF-8 with 'ParserInvalidUtf8'.
* Trims each statement span to exclude leading/trailing trivia and terminators,
  preserving lexical-error suffix units and extending trailing unparsed EOF units
  to the end of the source.
* Disables @ctxPureTableEnabled@ script-wide whenever any routine DDL or parse/lex
  error appears anywhere in the script.
* Sets 'unitCannotProduceResult' via the strict allowlist
  'statementCannotProduceResult' (forced 'False' whenever any lexical or parse
  error is present).
* Sets 'unitFacts' to 'Nothing' when the unit has a parse error or
  'parseSpanOnly' is 'True'; respects 'parseSyntaxOnly' (ignored when
  @parseColumnIR || parseColumnBundles@).
* Runs 'assembleColumns' only when 'parseColumnBundles' is 'True' and
  'parseSpanOnly' is 'False'.
* Validates all unit, edit, and column byte spans against UTF-8 boundaries.
-}
parseFrontend :: ParseOptions -> ByteString -> Either ParserError ParsedSQL
parseFrontend options sql = do
    splitUnits <- case splitSQL profile sql of
        Left err -> Left (ParserError ParserInvalidUtf8 (lexErrorMessage err))
        Right units -> Right (filter (isNothing . splitUnitDelimiterDirective) units)
    let !hasLexError = any (isJust . splitUnitLexError) splitUnits
        !sqlLen = BS.length sql
        pending = parsePending profile sqlLen hasLexError splitUnits
        !pureTableEnabled =
            not hasLexError
                && all (either (const False) (not . isRoutineDDL) . pendingResult) pending
    units <- buildUnits options sql hasLexError pureTableEnabled pending
    let rules = identifierRulesFor (parseDialect options) (parseServerConfig options)
        rawColumns =
            if parseColumnBundles options && not (parseSpanOnly options)
                then
                    assembleColumnsWith
                        sql
                        ( zipWith
                            ( \unit p ->
                                ( unit
                                , either
                                    (const Nothing)
                                    (Just . PreParsedUnit profile rules (pendingTokens p))
                                    (pendingResult p)
                                )
                            )
                            units
                            pending
                        )
                else []
    columns <- validateColumns sql units rawColumns
    pure (ParsedSQL (sessionInit options) units columns)
    where
        profile = dialectProfile (parseDialect options)

-- | Compute initial 'Core.SessionInit' from 'ParseOptions', folding configured identifiers.
sessionInit :: ParseOptions -> Core.SessionInit
sessionInit options =
    Core.SessionInit
        { Core.sessionNamespacePrefix =
            map (sessionConfigIdent rules) (serverNamespacePrefix server)
        , Core.sessionSearchPath =
            map (sessionSearchPathEntry rules) (serverSearchPath server)
        , Core.sessionCurrentUser =
            fmap (sessionConfigIdent rules) (serverCurrentUser server)
        , Core.sessionAutocommit = serverAutocommit server
        , Core.sessionForeignKeyChecks =
            fromMaybe
                (semDefaultForeignKeyChecks (dialectSemantics profile))
                (serverForeignKeyChecks server)
        , Core.sessionVariables = serverVariables server
        }
    where
        dialect = parseDialect options
        profile = dialectProfile dialect
        server = parseServerConfig options
        rules = identifierRulesFor dialect server

sessionConfigIdent :: IdentifierRules -> Text -> Core.Ident
sessionConfigIdent rules raw =
    case literalIdentifierPart (T.strip raw) of
        Just (identText, quoted) -> foldIdentText rules IdentRelation quoted identText
        Nothing -> Core.Ident raw

sessionSearchPathEntry :: IdentifierRules -> Text -> Core.SearchPathEntry
sessionSearchPathEntry rules raw =
    let trimmed = T.strip raw
     in if trimmed == "$user" || trimmed == "\"$user\""
            then Core.SearchPathCurrentUser
            else Core.SearchPathName (Core.Name [sessionConfigIdent rules trimmed])

parsePending :: DialectProfile -> Int -> Bool -> [SplitUnit] -> [PendingUnit]
parsePending profile sqlLen hasLexError = go
    where
        go [] = []
        go (su : rest) = parseOne su (null rest) : go rest

        parseOne su isLast
            | Just lexErr <- splitUnitLexError su =
                PendingUnit
                    { pendingStart = spanStart (splitUnitSpan su)
                    , pendingEnd = spanEnd (splitUnitSpan su)
                    , pendingTokens = splitUnitTokens su
                    , pendingResult = Left (lexErrorMessage lexErr)
                    }
            | otherwise =
                let ByteSpan start end = splitUnitSpan su
                 in case parseUnit profile su of
                        Right (L _ stmt) ->
                            PendingUnit
                                { pendingStart = start
                                , pendingEnd = end
                                , pendingTokens = splitUnitTokens su
                                , pendingResult = Right stmt
                                }
                        Left parseErr ->
                            let end' =
                                    if isLast && isNothing (splitUnitDelimiterSpan su) && not hasLexError
                                        then sqlLen
                                        else end
                             in PendingUnit
                                    { pendingStart = start
                                    , pendingEnd = end'
                                    , pendingTokens = splitUnitTokens su
                                    , pendingResult = Left (parseErrorMessage parseErr)
                                    }

buildUnits ::
    ParseOptions ->
    ByteString ->
    Bool ->
    Bool ->
    [PendingUnit] ->
    Either ParserError [StatementUnit]
buildUnits options sql hasLexError pureTableEnabled = go 0 1 []
    where
        !sqlLen = BS.length sql

        go _ _ acc [] = Right (reverse acc)
        go !prevEnd !idx acc (unit : rest)
            | start < 0 || start >= end || end > sqlLen || start < prevEnd =
                Left (ParserError ParserInvalidSourceRange "frontend returned an invalid source byte range")
            | not (isUtf8Boundary sql start && isUtf8Boundary sql end) =
                Left (ParserError ParserInvalidSourceRange "frontend returned a source range splitting UTF-8")
            | otherwise = do
                mFacts <- analyzeUnit unit
                let !statement =
                        Core.Statement
                            (Core.StatementId idx)
                            (Core.Span start end)
                            (unsafeSliceSpan (ByteSpan start end) sql)
                    !cannotProduceResult =
                        not hasLexError
                            && either (const False) statementCannotProduceResult (pendingResult unit)
                    !parseErr = either Just (const Nothing) (pendingResult unit)
                    !built =
                        StatementUnit
                            { unitStatement = statement
                            , unitFacts = mFacts
                            , unitParseError = parseErr
                            , unitCannotProduceResult = cannotProduceResult
                            }
                go end (idx + 1) (built : acc) rest
            where
                !start = pendingStart unit
                !end = pendingEnd unit

        analyzeUnit unit
            | parseSpanOnly options = Right Nothing
            | Left _ <- pendingResult unit = Right Nothing
            | Right rawStmt <- pendingResult unit =
                let !start = pendingStart unit
                    !end = pendingEnd unit
                    ctx =
                        makeAnalysisContext
                            options
                            sql
                            (ByteSpan start end)
                            (pendingTokens unit)
                            pureTableEnabled
                    stmt = unwrapExplainAnalyze rawStmt
                    sem =
                        if ctxSyntaxOnly ctx && not (ctxColumnIR ctx)
                            then emptySemanticFacts
                            else statementSemantics ctx stmt
                    edits =
                        (if ctxSyntaxEdits ctx then statementEdits ctx stmt else [])
                            ++ (if ctxColumnIR ctx then columnEdits ctx stmt else [])
                 in if all (validEdit start end) edits
                        then Right (Just (StatementFacts sem edits))
                        else Left (ParserError ParserInvalidEditRange "frontend returned an invalid edit range")

        validEdit !unitStart !unitEnd edit =
            parserEditStart edit >= unitStart
                && parserEditStart edit < parserEditEnd edit
                && parserEditEnd edit <= unitEnd
                && isUtf8Boundary sql (parserEditStart edit)
                && isUtf8Boundary sql (parserEditEnd edit)

{- | Strict allowlist of top-level SQL statements whose grammar production
cannot return a query result set in SQLite or DuckDB regardless of prior
schema, triggers, views, or session context.
-}
statementCannotProduceResult :: AST.Statement -> Bool
statementCannotProduceResult = \case
    AST.StatementCreate (L _ create) -> case locValue (AST.createObject create) of
        AST.CreateTableObject ct -> isNothing (AST.createTableAs ct)
        AST.CreateIndexObject _ -> True
        AST.CreateViewObject _ -> True
        AST.CreateTriggerObject _ -> True
        AST.CreateSchemaObject _ -> True
        _ -> False
    AST.StatementDrop (L _ dropStmt) -> case locValue (AST.dropObject dropStmt) of
        AST.DropTable -> True
        AST.DropIndex -> True
        AST.DropView -> True
        AST.DropMaterializedView -> True
        AST.DropSequence -> True
        AST.DropSchema -> True
        AST.DropDatabase -> True
        AST.DropTrigger -> True
        _ -> False
    AST.StatementAlter (L _ alterStmt) -> AST.alterObject alterStmt == AST.AlterObjectTable
    AST.StatementTransaction _ -> True
    AST.StatementInsert (L _ ins) -> isNothing (AST.insertWith ins) && null (AST.insertReturning ins)
    AST.StatementUpdate (L _ upd) -> isNothing (AST.updateWith upd) && null (AST.updateReturning upd)
    AST.StatementDelete (L _ del) -> isNothing (AST.deleteWith del) && null (AST.deleteReturning del)
    _ -> False

{- | True when a statement defines or drops a user-defined routine (function,
procedure, or macro), disabling script-wide pure-builtin assumptions.
-}
isRoutineDDL :: AST.Statement -> Bool
isRoutineDDL = \case
    AST.StatementCreate (L _ create) -> case locValue (AST.createObject create) of
        AST.CreateFunctionObject _ -> True
        AST.CreateProcedureObject _ -> True
        AST.CreateMacroObject _ -> True
        AST.CreateAggregateObject _ -> True
        _ -> False
    AST.StatementDrop (L _ dropStmt) -> case locValue (AST.dropObject dropStmt) of
        AST.DropFunction -> True
        AST.DropProcedure -> True
        AST.DropMacro -> True
        AST.DropAggregate -> True
        _ -> False
    _ -> False
