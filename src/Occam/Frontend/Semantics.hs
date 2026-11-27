{- | Pure-Haskell semantic fact extraction from the frontend AST.

Re-exports 'statementSemantics' and 'emptySemanticFacts' backed by the
modular submodules under @Occam.Frontend.Semantics.*@.
-}
module Occam.Frontend.Semantics (
    statementSemantics,
    emptySemanticFacts,
) where

import Data.Set qualified as Set
import Data.Text.Encoding qualified as TE
import Occam.Core qualified as Core
import Occam.Frontend.AST qualified as AST
import Occam.Frontend.Context (
    AnalysisContext (..),
    relationRef,
    unitSource,
    unwrapExplainAnalyze,
 )
import Occam.Frontend.Semantics.DDL (
    alterFacts,
    analyzeFacts,
    createDatabaseFacts,
    createIndexFacts,
    createMacroFacts,
    createRuleFacts,
    createSchemaFacts,
    createSequenceFacts,
    createStatisticsFacts,
    createTableFacts,
    createTriggerFacts,
    createViewFacts,
    createVirtualTableFacts,
    dropFacts,
    implicitCommit,
    renameTableFacts,
 )
import Occam.Frontend.Semantics.DML (
    copyFacts,
    deleteFacts,
    insertFacts,
    mergeFacts,
    truncateFacts,
    updateFacts,
    valuesMarkerSpans,
 )
import Occam.Frontend.Semantics.Query (
    VisitorState (..),
    addUnmodeledBarriers,
    analyzeNested,
    classifyQueryReadMode,
    isMySqlUserVarSelectInto,
    mergeNested,
    runAnalysisVisitor,
    selectIntoDefinition,
    visitorReads,
 )
import Occam.Frontend.Semantics.Sequence (
    SequenceState (..),
    initialSequenceState,
    sequenceEffectsForStatement,
 )
import Occam.Frontend.Semantics.Session (
    pragmaFacts,
    resetFacts,
    setFacts,
    transactionFacts,
    useFacts,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..))
import Occam.Parser.Dialect (DialectProfile (..), SemanticsProfile (..))
import Occam.Parser.Types (SQLDialect (..))

-- | Empty 'Core.SemanticFacts' with no catalog ops, session effects, tx effects, or barriers.
emptySemanticFacts :: Core.SemanticFacts
emptySemanticFacts = Core.SemanticFacts [] [] [] [] []

{- | Extract 'Core.SemanticFacts' for a single parsed statement under @ctx@.

Respects @ctxSyntaxOnly@ (when @not (ctxColumnIR ctx)@, returns
'emptySemanticFacts') and unwraps @EXPLAIN ANALYZE@ recursively.
-}
statementSemantics :: AnalysisContext -> AST.Statement -> Core.SemanticFacts
statementSemantics ctx rawStmt
    | ctxSyntaxOnly ctx && not (ctxColumnIR ctx) = emptySemanticFacts
    | otherwise = analyzeStatement ctx (unwrapExplainAnalyze rawStmt)

analyzeStatement :: AnalysisContext -> AST.Statement -> Core.SemanticFacts
analyzeStatement ctx stmt =
    let dialect = ctxDialect ctx
        isPlainExplain = case stmt of
            AST.StatementExplain _ -> True
            _ -> False
        needSharedSeqs = case stmt of
            AST.StatementQuery _ -> True
            AST.StatementInsert _ -> True
            AST.StatementUpdate _ -> True
            AST.StatementDelete _ -> True
            AST.StatementMerge _ -> True
            AST.StatementCopy _ -> True
            _ -> False
        seqIgnoredRoots = case stmt of
            AST.StatementInsert (L _ ins)
                | ctxPureTableEnabled ctx
                , dialect `elem` [MySQL, MariaDB]
                , Just assigns <- AST.insertOnDuplicate ins ->
                    valuesMarkerSpans assigns
            _ -> []
        mInitSeq = if needSharedSeqs then Just (initialSequenceState seqIgnoredRoots) else Nothing
        visitor = runAnalysisVisitor ctx (not isPlainExplain) [] [] mInitSeq stmt
        seqState = case vsSequences visitor of
            Just s -> s
            Nothing -> sequenceEffectsForStatement ctx stmt
        seqTransactional =
            semSequenceTransactional (dialectSemantics (ctxProfile ctx))
                && dialect /= PostgreSQL
        seqOps =
            [ Core.WriteSequenceOp (Core.SequenceWrite s seqTransactional)
            | s <- seqWrites seqState
            ]
                ++ [ Core.ReadOp (Core.ReadFact s Core.ReadData [] False)
                   | s <- seqReads seqState
                   ]
        allowsDeferredCall = case stmt of
            AST.StatementQuery _ -> True
            AST.StatementInsert _ -> True
            AST.StatementUpdate _ -> True
            AST.StatementDelete _ -> True
            AST.StatementMerge _ -> True
            AST.StatementCopy _ -> True
            AST.StatementExplain _ -> True
            _ -> False
        deferredFn = vsHasFunction visitor && not allowsDeferredCall
        fnBarriers =
            [ Core.ScopedBarrier
                Core.GlobalScope
                Core.UnknownStatementBarrier
                (Just "unmodeled routine or deferred expression effects")
            | seqUnknown seqState || deferredFn
            ]
        selectIntoOutsideQuery =
            vsSelectIntoCount visitor > 0
                && case stmt of
                    AST.StatementQuery _ -> False
                    AST.StatementExplain _ -> False
                    _ -> True
        selectIntoOutsideBarriers =
            [ Core.ScopedBarrier
                Core.GlobalScope
                Core.UnknownDMLBarrier
                (Just "SELECT INTO outside a standalone query")
            | selectIntoOutsideQuery
            ]
        txCommitEffs = [Core.TxCommit False | implicitCommit ctx stmt]
        baseFacts =
            Core.SemanticFacts
                { Core.factOps = seqOps
                , Core.factSession = []
                , Core.factPreTx = txCommitEffs
                , Core.factPostTx = txCommitEffs
                , Core.factBarriers = fnBarriers ++ selectIntoOutsideBarriers
                }
        (factsWithBody, inertPragma) = dispatchStatement ctx visitor stmt baseFacts
        hasUnmodeledSession =
            not inertPragma
                && any
                    ( \case
                        Core.SetVariable {} -> True
                        Core.ResetVariable {} -> True
                        Core.ResetAll {} -> True
                        _ -> False
                    )
                    (Core.factSession factsWithBody)
        sessionBarriers =
            [ Core.ScopedBarrier
                Core.GlobalScope
                Core.UnknownSessionEffectBarrier
                (Just "unmodeled session variable effects")
            | hasUnmodeledSession
            ]
        userVarBarriers =
            [ Core.ScopedBarrier
                (Core.UserVariableScope varName)
                Core.UnsupportedExpressionBarrier
                (Just ("user variable " <> Core.identText varName))
            | varName <- vsUserVars visitor
            , not
                (any (\b -> Core.barrierScope b == Core.UserVariableScope varName) (Core.factBarriers factsWithBody))
            ]
        dedupedOps = dedupSequenceWrites (Core.factOps factsWithBody)
     in factsWithBody
            { Core.factOps = dedupedOps
            , Core.factBarriers = Core.factBarriers factsWithBody ++ sessionBarriers ++ userVarBarriers
            }

dedupSequenceWrites :: [Core.CatalogOp] -> [Core.CatalogOp]
dedupSequenceWrites = reverse . fst . foldl' step ([], Set.empty)
    where
        step (acc, seen) op@(Core.WriteSequenceOp sw) =
            let target = Core.sequenceWriteTarget sw
             in if Set.member target seen
                    then (acc, seen)
                    else (op : acc, Set.insert target seen)
        step (acc, seen) op = (op : acc, seen)

dispatchStatement ::
    AnalysisContext ->
    VisitorState ->
    AST.Statement ->
    Core.SemanticFacts ->
    (Core.SemanticFacts, Bool)
dispatchStatement ctx visitor stmt baseFacts = case stmt of
    AST.StatementQuery (L _ query) ->
        (queryFacts ctx visitor query baseFacts, False)
    AST.StatementInsert (L sp ins) ->
        (insertFacts analyzeStatement ctx visitor sp ins baseFacts, False)
    AST.StatementUpdate (L sp upd) ->
        (updateFacts analyzeStatement ctx visitor sp upd baseFacts, False)
    AST.StatementDelete (L sp del) ->
        (deleteFacts analyzeStatement ctx visitor sp del baseFacts, False)
    AST.StatementMerge (L sp m) ->
        (mergeFacts analyzeStatement ctx visitor sp m baseFacts, False)
    AST.StatementTruncate (L _ tr) ->
        (truncateFacts ctx tr baseFacts, False)
    AST.StatementCopy (L _ cp) ->
        (copyFacts analyzeStatement ctx visitor cp baseFacts, False)
    AST.StatementCreate (L _ cr) ->
        (createFacts ctx visitor cr baseFacts, False)
    AST.StatementDrop (L _ dr) ->
        (dropFacts ctx dr baseFacts, False)
    AST.StatementAlter (L _ alt) ->
        (alterFacts ctx visitor alt baseFacts, False)
    AST.StatementRenameTable (L _ renames) ->
        (renameTableFacts ctx renames baseFacts, False)
    AST.StatementAnalyze (L _ an) ->
        (analyzeFacts ctx an baseFacts, False)
    AST.StatementSet (L _ setStmt) ->
        (setFacts analyzeStatement ctx visitor setStmt baseFacts, False)
    AST.StatementReset (L _ resetStmt) ->
        (resetFacts ctx resetStmt baseFacts, False)
    AST.StatementUse (L sp name) ->
        (useFacts ctx (L sp name) baseFacts, False)
    AST.StatementPragma (L _ pr) ->
        pragmaFacts ctx pr baseFacts
    AST.StatementTransaction (L _ tx) ->
        (transactionFacts ctx tx baseFacts, False)
    AST.StatementExplain _ ->
        let readFacts = visitorReads visitor [] Core.ReadSchema
         in ( baseFacts
                { Core.factOps = Core.factOps baseFacts ++ map Core.ReadOp readFacts
                , Core.factBarriers = addUnmodeledBarriers visitor [] (Core.factBarriers baseFacts)
                }
            , False
            )
    AST.StatementOptimize (L _ opt)
        | ctxDialect ctx == ClickHouse
        , L _ table <- AST.optimizeTable opt
        , length (AST.nameParts table) `elem` [1, 2]
        , Just target <- relationRef ctx table ->
            ( baseFacts
                { Core.factOps =
                    Core.factOps baseFacts ++ [Core.ReadOp (Core.ReadFact target Core.ReadData [] False)]
                , Core.factBarriers =
                    Core.factBarriers baseFacts
                        ++ [Core.ScopedBarrier (Core.RelationScope target) Core.UnknownAlterBarrier (Just "OPTIMIZE TABLE")]
                }
            , False
            )
    AST.StatementRefreshMaterializedView (L _ rmv) ->
        case relationRef ctx (locValue (AST.refreshName rmv)) of
            Just target ->
                let mode = if AST.refreshWithData rmv then Core.ReadData else Core.ReadSchema
                 in ( baseFacts
                        { Core.factOps =
                            Core.factOps baseFacts ++ [Core.RefreshCatalogOp (Core.RefreshOp target mode)]
                        }
                    , False
                    )
            Nothing ->
                ( baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [Core.ScopedBarrier Core.GlobalScope Core.UnknownViewBarrier (Just "REFRESH MATERIALIZED VIEW")]
                    }
                , False
                )
    _ ->
        ( baseFacts
            { Core.factBarriers =
                Core.factBarriers baseFacts
                    ++ [ Core.ScopedBarrier
                            Core.GlobalScope
                            Core.UnknownStatementBarrier
                            (Just (TE.decodeUtf8 (unitSource ctx)))
                       ]
            }
        , False
        )

queryFacts ::
    AnalysisContext -> VisitorState -> AST.Query -> Core.SemanticFacts -> Core.SemanticFacts
queryFacts ctx visitor query baseFacts =
    let nested = analyzeNested analyzeStatement ctx False (AST.StatementQuery (L (ByteSpan 0 0) query))
        readMode =
            if null nested && vsSelectIntoCount visitor == 0
                then classifyQueryReadMode ctx query visitor
                else Core.ReadData
        readFacts = visitorReads visitor [] readMode
        mIntoTable = case locValue (AST.queryBody query) of
            AST.QueryBodySelect sc | vsSelectIntoCount visitor == 1 -> selectIntoDefinition ctx sc
            _ -> Nothing
        (ops', intoBarriers) = case mIntoTable of
            Just tbl -> ([Core.DefineTableOp tbl {Core.tableSources = readFacts}], [])
            Nothing ->
                let badInto =
                        vsSelectIntoCount visitor > 0
                            && not (vsSelectIntoCount visitor == 1 && isMySqlUserVarSelectInto ctx query)
                    b =
                        [ Core.ScopedBarrier
                            Core.GlobalScope
                            Core.UnknownDMLBarrier
                            (Just "unmodeled SELECT INTO target, dialect, or nested context")
                        | badInto
                        ]
                 in (map Core.ReadOp readFacts, b)
        barriers' = addUnmodeledBarriers visitor [] (Core.factBarriers baseFacts ++ intoBarriers)
        facts1 =
            baseFacts
                { Core.factOps = Core.factOps baseFacts ++ ops'
                , Core.factBarriers = barriers'
                }
     in mergeNested facts1 nested

createFacts ::
    AnalysisContext -> VisitorState -> AST.CreateStmt -> Core.SemanticFacts -> Core.SemanticFacts
createFacts ctx visitor cr baseFacts = case locValue (AST.createObject cr) of
    AST.CreateTableObject ct -> createTableFacts analyzeStatement ctx visitor cr ct baseFacts
    AST.CreateViewObject cv -> createViewFacts analyzeStatement ctx visitor cr cv baseFacts
    AST.CreateIndexObject ci -> createIndexFacts ctx visitor cr ci baseFacts
    AST.CreateTriggerObject tr -> createTriggerFacts analyzeStatement ctx cr tr baseFacts
    AST.CreateSequenceObject cs -> createSequenceFacts ctx cr cs baseFacts
    AST.CreateSchemaObject cs -> createSchemaFacts ctx cs baseFacts
    AST.CreateDatabaseObject db -> createDatabaseFacts ctx cr db baseFacts
    AST.CreateMacroObject cm -> createMacroFacts ctx visitor cm baseFacts
    AST.CreateVirtualTableObject cvt -> createVirtualTableFacts ctx visitor cvt baseFacts
    AST.CreateRuleObject crRule -> createRuleFacts analyzeStatement ctx visitor crRule baseFacts
    AST.CreateStatisticsObject cs -> createStatisticsFacts ctx cs baseFacts
    _ ->
        baseFacts
            { Core.factBarriers =
                Core.factBarriers baseFacts
                    ++ [ Core.ScopedBarrier
                            Core.GlobalScope
                            Core.UnknownStatementBarrier
                            (Just (TE.decodeUtf8 (unitSource ctx)))
                       ]
            }
