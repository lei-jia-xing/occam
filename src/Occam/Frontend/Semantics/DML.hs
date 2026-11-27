{-# LANGUAGE TupleSections #-}

-- | Semantic fact extraction for DML statements (INSERT, UPDATE, DELETE, MERGE, TRUNCATE, COPY).
module Occam.Frontend.Semantics.DML (
    valuesMarkerSpans,
    insertFacts,
    updateFacts,
    deleteFacts,
    mergeFacts,
    truncateFacts,
    copyFacts,
) where

import Control.Applicative ((<|>))
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Occam.Core qualified as Core
import Occam.Frontend.AST qualified as AST
import Occam.Frontend.Context (
    AnalysisContext (..),
    IdentifierClass (..),
    canonicalIdent,
    canonicalIdentText,
    nameFromAST,
    nameParts,
    relationRef,
    unitSource,
    unquoteNamePart,
    unsafeSliceSource,
 )
import Occam.Frontend.Semantics.Expr (conflictExprColumns, duplicateExprColumns)
import Occam.Frontend.Semantics.Pure (isValuesMarker)
import Occam.Frontend.Semantics.Query (
    TableSource (..),
    VisitorState (..),
    addUnmodeledBarriers,
    analyzeNested,
    findSource,
    fromSources,
    mergeNested,
    sourceFromName,
    tableFactorSource,
    tableWithJoinsSources,
    updateVisitorReads,
    visitorReads,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..))

-- | Collect spans of MySQL @VALUES(col)@ proposed-row markers in @ON DUPLICATE KEY UPDATE@ assignments.
valuesMarkerSpans :: [Located AST.Assignment] -> [ByteSpan]
valuesMarkerSpans = concatMap (go . AST.assignValue . locValue)
    where
        go (L sp expr) = case expr of
            AST.ExprCall (L _ fn)
                | isValuesMarker fn -> sp : concatMap argSpans (AST.functionArgs fn)
                | otherwise -> concatMap argSpans (AST.functionArgs fn)
            AST.ExprUnary _ e -> go e
            AST.ExprBinary _ l r -> go l ++ go r
            AST.ExprParen e -> go e
            AST.ExprCast _ e _ -> go e
            AST.ExprCase (AST.CaseExpr mOp whens mElse) ->
                maybe [] go mOp ++ concatMap (\(c, r) -> go c ++ go r) whens ++ maybe [] go mElse
            _ -> []
        argSpans (L _ (AST.FunctionArgExpr e)) = go e
        argSpans (L _ (AST.FunctionArgNamed _ e)) = go e
        argSpans _ = []

insertConflict :: AST.InsertStmt -> Core.ConflictAction
insertConflict ins
    | AST.insertReplace ins = Core.ConflictReplace
    | Just (L _ act) <- AST.insertOrAction ins = case act of
        AST.InsertOrReplace -> Core.ConflictReplace
        AST.InsertOrIgnore -> Core.ConflictIgnore
        AST.InsertOrRollback -> Core.ConflictError
        AST.InsertOrAbort -> Core.ConflictError
        AST.InsertOrFail -> Core.ConflictError
    | isJust (AST.insertOnDuplicate ins) = Core.ConflictUpdate
    | Just (L _ occ) <- AST.insertOnConflict ins = case AST.onConflictAction occ of
        AST.OnConflictDoNothing -> Core.ConflictIgnore
        AST.OnConflictDoUpdate _ _ -> Core.ConflictUpdate
    | otherwise = Core.ConflictError

insertColumns :: AnalysisContext -> AST.InsertStmt -> [Core.Ident]
insertColumns ctx ins =
    let explicit = map (canonicalIdent ctx IdentColumn . locValue) (AST.insertColumns ins)
        extra = case (AST.insertOnDuplicate ins, AST.insertOnConflict ins) of
            (Just assigns, _) -> assignTargetCols assigns
            (_, Just (L _ occ)) -> case AST.onConflictAction occ of
                AST.OnConflictDoUpdate assigns _ -> assignTargetCols assigns
                AST.OnConflictDoNothing -> []
            _ -> []
     in dedupPreserve (explicit ++ extra)
    where
        assignTargetCols assigns =
            [ col
            | L _ asgn <- assigns
            , Just name <- [nameFromAST ctx IdentColumn (locValue (AST.assignTarget asgn))]
            , Just col <- [listToMaybe (reverse (Core.nameParts name))]
            ]

conflictReadColumns :: AnalysisContext -> AST.OnConflictClause -> [Core.Ident]
conflictReadColumns ctx occ =
    let arbiterCols =
            [ canonicalIdent ctx IdentColumn part
            | L _ kp <- AST.onConflictKeys occ
            , AST.ExprColumn (L _ (AST.Name [L _ part])) <- [locValue (AST.indexKeyExpr kp)]
            ]
        predCols = maybe [] (conflictExprColumns ctx . locValue) (AST.onConflictWhere occ)
        updateCols = case AST.onConflictAction occ of
            AST.OnConflictDoNothing -> []
            AST.OnConflictDoUpdate assigns mSel ->
                concatMap (conflictExprColumns ctx . locValue . AST.assignValue . locValue) assigns
                    ++ maybe [] (conflictExprColumns ctx . locValue) mSel
     in dedupPreserve (arbiterCols ++ predCols ++ updateCols)

duplicateReadColumns ::
    AnalysisContext -> AST.InsertStmt -> [Located AST.Assignment] -> [Core.Ident]
duplicateReadColumns ctx ins assigns =
    let keyCols = map (canonicalIdent ctx IdentColumn . locValue) (AST.insertColumns ins)
        rowAlias = unquoteNamePart . locValue . AST.aliasName . locValue <$> AST.insertValuesAlias ins
        exprCols =
            concatMap
                (\a -> duplicateExprColumns ctx (locValue (AST.assignValue (locValue a))) rowAlias)
                assigns
     in dedupPreserve (keyCols ++ exprCols)

mergeTargetReads :: [Core.ReadFact] -> Core.NameRef -> [Core.Ident] -> [Core.ReadFact]
mergeTargetReads reads0 target extra
    | null extra = reads0
    | any matchesTarget reads0 = map extendMatching reads0
    | otherwise = reads0 ++ [Core.ReadFact target Core.ReadData extra False]
    where
        matchesTarget r = Core.readMode r == Core.ReadData && Core.readSource r == target
        extendMatching r
            | matchesTarget r =
                let existing = Core.readColumns r
                    added = [c | c <- extra, c `notElem` existing]
                 in r {Core.readColumns = existing ++ added}
            | otherwise = r

insertFacts ::
    (AnalysisContext -> AST.Statement -> Core.SemanticFacts) ->
    AnalysisContext ->
    VisitorState ->
    ByteSpan ->
    AST.InsertStmt ->
    Core.SemanticFacts ->
    Core.SemanticFacts
insertFacts analyzeStmt ctx visitor stmtSpan ins baseFacts =
    let L tblSpan tblName = AST.insertTable ins
        nested = analyzeNested analyzeStmt ctx True (AST.StatementInsert (L stmtSpan ins))
        excluded = [tblSpan]
     in case relationRef ctx tblName of
            Nothing ->
                baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [ Core.ScopedBarrier
                                    Core.GlobalScope
                                    Core.UnknownDMLBarrier
                                    (Just (TE.decodeUtf8 (unsafeSliceSource ctx tblSpan)))
                               ]
                    }
            Just target ->
                let reads0 = visitorReads visitor excluded Core.ReadData
                    reads1 = case (AST.insertOnConflict ins, AST.insertOnDuplicate ins) of
                        (Just (L _ occ), _) -> mergeTargetReads reads0 target (conflictReadColumns ctx occ)
                        (_, Just assigns) -> mergeTargetReads reads0 target (duplicateReadColumns ctx ins assigns)
                        _ -> reads0
                    dw =
                        Core.DataWrite
                            { Core.dataWriteTarget = target
                            , Core.dataWriteKind = Core.InsertWrite (insertConflict ins)
                            , Core.dataWriteColumns = insertColumns ctx ins
                            , Core.dataWriteUnqualifiedColumns = []
                            , Core.dataWriteReads = reads1
                            , Core.dataWriteOnly = False
                            }
                    partitionBarriers =
                        [ Core.ScopedBarrier
                            Core.GlobalScope
                            Core.UnknownDMLBarrier
                            (Just "INSERT PARTITION")
                        | not (null (AST.insertPartitions ins))
                        ]
                    facts1 =
                        baseFacts
                            { Core.factOps = Core.factOps baseFacts ++ [Core.WriteDataOp dw]
                            , Core.factBarriers =
                                addUnmodeledBarriers visitor excluded (Core.factBarriers baseFacts) ++ partitionBarriers
                            }
                 in mergeNested facts1 nested

updateFacts ::
    (AnalysisContext -> AST.Statement -> Core.SemanticFacts) ->
    AnalysisContext ->
    VisitorState ->
    ByteSpan ->
    AST.UpdateStmt ->
    Core.SemanticFacts ->
    Core.SemanticFacts
updateFacts analyzeStmt ctx visitor stmtSpan upd baseFacts =
    let fromList = maybe [] (fromSources ctx) (AST.updateFrom upd)
        targetItems = AST.fromItems (AST.updateTargets upd)
        mPrimary = listToMaybe targetItems >>= tableFactorSource ctx . locValue
        initialTargets :: [(TableSource, Maybe ByteSpan)]
        initialTargets = case mPrimary of
            Just p
                | isNothing (tsAlias p)
                , length (tsParts p) == 1
                , Just src <- findSource ctx (tsName p) fromList ->
                    [(src, Just (tsSpan p))]
            _ -> [(src, Nothing) | L _ tr <- targetItems, src <- tableWithJoinsSources ctx tr]
        assignments =
            mapMaybe
                (assignmentColumnTarget . locValue . AST.assignTarget . locValue)
                (AST.updateAssignments upd)
        attributed =
            [ (src, mDesig, cols, unqualCols)
            | (src, mDesig) <- initialTargets
            , let (cols, unqualCols) = splitCols src assignments
            , not (null cols && null unqualCols)
            ]
        resolved =
            mapMaybe
                ( \(src, _, cols, unqualCols) ->
                    (,cols,unqualCols,tsOnly src) <$> relationRef ctx (tsName src)
                )
                attributed
        nested = analyzeNested analyzeStmt ctx True (AST.StatementUpdate (L stmtSpan upd))
        excluded = mapMaybe (\(_, mD, _, _) -> mD) attributed
     in if null attributed
            then
                baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [Core.ScopedBarrier Core.GlobalScope Core.UnknownDMLBarrier (Just (TE.decodeUtf8 (unitSource ctx)))]
                    }
            else
                let targetSpans = [tsSpan src | (src, _, _, _) <- attributed]
                    isAll =
                        isNothing (AST.updateWhere upd)
                            && isNothing (AST.updateFrom upd)
                            && length targetItems == 1
                            && all (null . AST.tableRefJoins . locValue) targetItems
                            && isNothing (AST.updateLimit upd)
                    readFacts = updateVisitorReads visitor excluded targetSpans isAll Core.ReadData
                    writes =
                        [ Core.WriteDataOp
                            Core.DataWrite
                                { Core.dataWriteTarget = ref
                                , Core.dataWriteKind =
                                    Core.UpdateWrite (if isAll then Core.UpdateAllRows else Core.UpdateFilteredRows)
                                , Core.dataWriteColumns = cols
                                , Core.dataWriteUnqualifiedColumns = unqualCols
                                , Core.dataWriteReads = readFacts
                                , Core.dataWriteOnly = onlyFlag
                                }
                        | (ref, cols, unqualCols, onlyFlag) <- resolved
                        ]
                    factsWithBarriers =
                        baseFacts
                            { Core.factBarriers = addUnmodeledBarriers visitor excluded (Core.factBarriers baseFacts)
                            }
                 in if isJust (AST.updateWith upd)
                        then
                            let merged = mergeNested factsWithBarriers nested
                             in merged {Core.factOps = Core.factOps merged ++ writes}
                        else
                            mergeNested
                                (factsWithBarriers {Core.factOps = Core.factOps factsWithBarriers ++ writes})
                                nested
    where
        assignmentColumnTarget astName = do
            folded <- nameParts ctx IdentColumn astName
            case reverse folded of
                colIdent : revQual ->
                    let qualTexts =
                            zipWith
                                (\_ (L _ p) -> canonicalIdentText ctx IdentRelation p)
                                revQual
                                (AST.nameParts astName)
                        qual = [canonicalIdentText ctx IdentRelation (locValue p) | p <- init (AST.nameParts astName)]
                     in Just (if null qualTexts then Nothing else Just qual, colIdent)
                [] -> Nothing
        splitCols src = foldl' step ([], [])
            where
                step (cs, ucs) (Nothing, c) =
                    if c `elem` ucs then (cs, ucs) else (cs, ucs ++ [c])
                step (cs, ucs) (Just qual, c)
                    | sourceMatchesQualifier src qual && c `notElem` cs = (cs ++ [c], ucs)
                    | otherwise = (cs, ucs)

sourceMatchesQualifier :: TableSource -> [Text] -> Bool
sourceMatchesQualifier src qual = case tsAlias src of
    Just al -> qual == [al]
    Nothing ->
        qual == tsParts src
            || (length qual == 1 && listToMaybe (reverse (tsParts src)) == listToMaybe qual)

deleteFacts ::
    (AnalysisContext -> AST.Statement -> Core.SemanticFacts) ->
    AnalysisContext ->
    VisitorState ->
    ByteSpan ->
    AST.DeleteStmt ->
    Core.SemanticFacts ->
    Core.SemanticFacts
deleteFacts analyzeStmt ctx visitor stmtSpan del baseFacts =
    let fromList =
            maybe
                (maybe [] (fromSources ctx) (AST.deleteUsing del))
                (fromSources ctx)
                (AST.deleteFromTables del)
        targets =
            if null (AST.deleteTargets del)
                then
                    [ (src {tsOnly = AST.deleteOnly del}, Nothing)
                    | Just src <- [sourceFromName ctx (locSpan (AST.deleteTable del)) (locValue (AST.deleteTable del))]
                    ]
                else
                    [ (src, Just sp)
                    | L _ dt <- AST.deleteTargets del
                    , let L sp name = AST.deleteTargetName dt
                    , Just src <- [findSource ctx name fromList <|> sourceFromName ctx sp name]
                    ]
        resolved =
            mapMaybe
                (\(src, _) -> (,tsOnly src) <$> relationRef ctx (tsName src))
                targets
        nested = analyzeNested analyzeStmt ctx True (AST.StatementDelete (L stmtSpan del))
        excluded = mapMaybe snd targets
     in if null resolved
            then
                baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [Core.ScopedBarrier Core.GlobalScope Core.UnknownDMLBarrier (Just (TE.decodeUtf8 (unitSource ctx)))]
                    }
            else
                let readFacts = visitorReads visitor excluded Core.ReadData
                    delScope = if isNothing (AST.deleteWhere del) then Core.DeleteAllRows else Core.DeleteFilteredRows
                    writes =
                        [ Core.WriteDataOp
                            Core.DataWrite
                                { Core.dataWriteTarget = ref
                                , Core.dataWriteKind = Core.DeleteWrite delScope
                                , Core.dataWriteColumns = []
                                , Core.dataWriteUnqualifiedColumns = []
                                , Core.dataWriteReads = readFacts
                                , Core.dataWriteOnly = onlyFlag
                                }
                        | (ref, onlyFlag) <- resolved
                        ]
                    factsWithBarriers =
                        baseFacts
                            { Core.factBarriers = addUnmodeledBarriers visitor excluded (Core.factBarriers baseFacts)
                            }
                 in if isJust (AST.deleteWith del)
                        then
                            let merged = mergeNested factsWithBarriers nested
                             in merged {Core.factOps = Core.factOps merged ++ writes}
                        else
                            mergeNested
                                (factsWithBarriers {Core.factOps = Core.factOps factsWithBarriers ++ writes})
                                nested

mergeFacts ::
    (AnalysisContext -> AST.Statement -> Core.SemanticFacts) ->
    AnalysisContext ->
    VisitorState ->
    ByteSpan ->
    AST.MergeStmt ->
    Core.SemanticFacts ->
    Core.SemanticFacts
mergeFacts analyzeStmt ctx visitor stmtSpan m baseFacts =
    let L tblSpan tblName = AST.mergeTarget m
        nested = analyzeNested analyzeStmt ctx True (AST.StatementMerge (L stmtSpan m))
     in case relationRef ctx tblName of
            Nothing ->
                baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [ Core.ScopedBarrier
                                    Core.GlobalScope
                                    Core.UnknownDMLBarrier
                                    (Just (TE.decodeUtf8 (unsafeSliceSource ctx tblSpan)))
                               ]
                    }
            Just target ->
                let readFacts = visitorReads visitor [] Core.ReadData
                    cols = mergeColumns ctx m
                    dw =
                        Core.DataWrite
                            { Core.dataWriteTarget = target
                            , Core.dataWriteKind = Core.MergeWrite
                            , Core.dataWriteColumns = cols
                            , Core.dataWriteUnqualifiedColumns = []
                            , Core.dataWriteReads = readFacts
                            , Core.dataWriteOnly = False
                            }
                    facts1 =
                        baseFacts
                            { Core.factOps = Core.factOps baseFacts ++ [Core.WriteDataOp dw]
                            , Core.factBarriers = addUnmodeledBarriers visitor [] (Core.factBarriers baseFacts)
                            }
                 in mergeNested facts1 nested

mergeColumns :: AnalysisContext -> AST.MergeStmt -> [Core.Ident]
mergeColumns ctx m = dedupPreserve (concatMap clauseCols (AST.mergeClauses m))
    where
        clauseCols (L _ cl) = case AST.mergeClauseAction cl of
            AST.MergeUpdate assigns ->
                [ col
                | L _ asgn <- assigns
                , Just name <- [nameFromAST ctx IdentColumn (locValue (AST.assignTarget asgn))]
                , Just col <- [listToMaybe (reverse (Core.nameParts name))]
                ]
            AST.MergeInsert cols _ ->
                map (canonicalIdent ctx IdentColumn . locValue) cols
            _ -> []

truncateFacts :: AnalysisContext -> AST.TruncateStmt -> Core.SemanticFacts -> Core.SemanticFacts
truncateFacts ctx tr baseFacts
    | null (AST.truncateTables tr) =
        baseFacts
            { Core.factBarriers =
                Core.factBarriers baseFacts
                    ++ [ Core.ScopedBarrier
                            Core.GlobalScope
                            Core.UnknownTruncateBarrier
                            (Just (TE.decodeUtf8 (unitSource ctx)))
                       ]
            }
    | otherwise = foldl' step baseFacts (AST.truncateTables tr)
    where
        behavior =
            if AST.truncateBehavior tr == Just AST.DropCascade
                then Core.TruncateCascade
                else Core.TruncateRestrict
        identity =
            if AST.truncateRestartIdentity tr == Just True
                then Core.RestartIdentity
                else Core.ContinueIdentity
        step acc (L sp tblName) = case relationRef ctx tblName of
            Just target ->
                let dw =
                        Core.DataWrite
                            { Core.dataWriteTarget = target
                            , Core.dataWriteKind = Core.TruncateWrite behavior identity
                            , Core.dataWriteColumns = []
                            , Core.dataWriteUnqualifiedColumns = []
                            , Core.dataWriteReads = []
                            , Core.dataWriteOnly = AST.truncateOnly tr
                            }
                 in acc {Core.factOps = Core.factOps acc ++ [Core.WriteDataOp dw]}
            Nothing ->
                acc
                    { Core.factBarriers =
                        Core.factBarriers acc
                            ++ [ Core.ScopedBarrier
                                    Core.GlobalScope
                                    Core.UnknownTruncateBarrier
                                    (Just (TE.decodeUtf8 (unsafeSliceSource ctx sp)))
                               ]
                    }

copyFacts ::
    (AnalysisContext -> AST.Statement -> Core.SemanticFacts) ->
    AnalysisContext ->
    VisitorState ->
    AST.CopyStmt ->
    Core.SemanticFacts ->
    Core.SemanticFacts
copyFacts analyzeStmt ctx visitor cp baseFacts =
    let extBarriers =
            [ Core.ScopedBarrier Core.GlobalScope Core.UnknownStatementBarrier (Just "external COPY effects")
            | isExternal (AST.copyTarget cp)
            ]
        acc0 = baseFacts {Core.factBarriers = Core.factBarriers baseFacts ++ extBarriers}
     in case (AST.copyDirection cp, AST.copySource cp) of
            (AST.CopyTo, AST.CopySourceTable (L sp tblName) cols) ->
                let colIdents = map (canonicalIdent ctx IdentColumn . locValue) cols
                 in case relationRef ctx tblName of
                        Just target ->
                            acc0
                                { Core.factOps =
                                    Core.factOps acc0 ++ [Core.ReadOp (Core.ReadFact target Core.ReadData colIdents False)]
                                }
                        Nothing ->
                            acc0
                                { Core.factBarriers =
                                    Core.factBarriers acc0
                                        ++ [ Core.ScopedBarrier
                                                Core.GlobalScope
                                                Core.UnknownDMLBarrier
                                                (Just (TE.decodeUtf8 (unsafeSliceSource ctx sp)))
                                           ]
                                }
            (AST.CopyTo, AST.CopySourceQuery q) ->
                let nested = analyzeNested analyzeStmt ctx False (AST.StatementQuery q)
                    readFacts = visitorReads visitor [] Core.ReadData
                    acc1 =
                        acc0
                            { Core.factOps = Core.factOps acc0 ++ map Core.ReadOp readFacts
                            , Core.factBarriers = addUnmodeledBarriers visitor [] (Core.factBarriers acc0)
                            }
                 in mergeNested acc1 nested
            (AST.CopyFrom, AST.CopySourceTable (L sp tblName) cols) ->
                let colIdents = map (canonicalIdent ctx IdentColumn . locValue) cols
                 in case relationRef ctx tblName of
                        Just target ->
                            let dw =
                                    Core.DataWrite
                                        { Core.dataWriteTarget = target
                                        , Core.dataWriteKind = Core.InsertWrite Core.ConflictError
                                        , Core.dataWriteColumns = colIdents
                                        , Core.dataWriteUnqualifiedColumns = []
                                        , Core.dataWriteReads = []
                                        , Core.dataWriteOnly = False
                                        }
                             in acc0 {Core.factOps = Core.factOps acc0 ++ [Core.WriteDataOp dw]}
                        Nothing ->
                            acc0
                                { Core.factBarriers =
                                    Core.factBarriers acc0
                                        ++ [ Core.ScopedBarrier
                                                Core.GlobalScope
                                                Core.UnknownDMLBarrier
                                                (Just (TE.decodeUtf8 (unsafeSliceSource ctx sp)))
                                           ]
                                }
            (AST.CopyFrom, AST.CopySourceQuery (L sp _)) ->
                acc0
                    { Core.factBarriers =
                        Core.factBarriers acc0
                            ++ [ Core.ScopedBarrier
                                    Core.GlobalScope
                                    Core.UnknownDMLBarrier
                                    (Just (TE.decodeUtf8 (unsafeSliceSource ctx sp)))
                               ]
                    }
    where
        isExternal (AST.CopyFile _) = True
        isExternal (AST.CopyProgram _) = True
        isExternal _ = False

dedupPreserve :: (Ord a) => [a] -> [a]
dedupPreserve = reverse . fst . foldl' step ([], Set.empty)
    where
        step (acc, seen) x
            | Set.member x seen = (acc, seen)
            | otherwise = (x : acc, Set.insert x seen)
