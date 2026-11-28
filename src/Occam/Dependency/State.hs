-- | Catalog, session, transaction, scan, and evaluation state.
module Occam.Dependency.State (
    -- Canonical catalog keys
    RelationKey (..),
    IndexKey (..),
    TriggerKey (..),
    SequenceKey (..),
    ConstraintIdentity (..),
    ConstraintKey (..),
    ObjectKey (..),
    BarrierKey (..),
    -- Catalog state
    RelationState (..),
    IndexState (..),
    TriggerState (..),
    SequenceState (..),
    ConstraintState (..),
    DefinitionState (..),
    Catalog (..),
    emptyCatalog,
    newRelation,
    -- Session and transaction state
    SessionState (..),
    sessionFromInit,
    effectiveNamespacePrefix,
    effectiveCurrentUser,
    effectiveSessionUser,
    effectiveSearchPath,
    clearTransactionSession,
    Savepoint (..),
    TxFrame (..),
    ScanState (..),
    initialScanState,
    transactionActive,
    transactionOpener,
    -- Evaluation accumulator
    Eval (..),
    initialEval,
    addDeps,
    addTouch,
    addTouches,
    addBarrier,
    ambiguousMutation,
    mutateResolved,
    evalToState,
    -- Pure catalog operations
    lookupRelation,
    insertRelation,
    deleteRelation,
    deleteIndex,
    deleteTrigger,
    deleteSequence,
    deleteConstraint,
    clearRelationObjects,
    -- Lenses and state modification combinators
    evalCatalogL,
    evalSessionL,
    evalDepsL,
    evalTouchesL,
    evalBarriersL,
    catalogRelationsL,
    catalogIndexesL,
    catalogTriggersL,
    catalogSequencesL,
    catalogConstraintsL,
    catalogTypesL,
    catalogSchemasL,
    catalogNextUnnamedConstraintL,
    relColumnsL,
    relDefinitionL,
    relTransactionalL,
    relOnCommitL,
    relOnCommitFrameIdL,
    relDataWritesL,
    relCardinalityWritesL,
    relColumnWritesL,
    relSchemaWritesL,
    relConstraintsL,
    relIndexesL,
    relTriggersL,
    relSourcesL,
    relUnresolvedSourcesL,
    relSourceReadsL,
    relPartitionParentsL,
    relInheritParentsL,
    relColumnLineageL,
    relConditionColumnsL,
    sequenceWritesL,
    sequenceNonTxWritesL,
    modifyCatalog,
    modifySession,
    modifyRelation,
    modifyEvalRelation,
    -- Write signatures and name utilities
    WriteSignature (..),
    writeSignature,
    nameSuffix,
    nameLast,
    rebuildForeignKeys,
    rebuildChildRelations,
    rebuildRelationsByLast,
) where

import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Lens.Micro (Lens', lens, (%~), (&))
import Occam.Core.Schema (Ident, Name (..))
import Occam.Core.Semantics (
    BarrierReason (..),
    BarrierScope (..),
    ScopedBarrier (..),
    SearchPathEntry (..),
    SessionInit (..),
    SessionValue (..),
 )
import Occam.Core.Types (StatementId)
import Occam.Dependency.State.Catalog
import Occam.Dependency.State.Lenses

-- Session and transaction state
data SessionState = SessionState
    { sessNamespacePrefix :: ![Ident]
    , sessSearchPath :: ![SearchPathEntry]
    , sessCurrentUser :: !(Maybe Ident)
    , -- The user that authenticated the connection. `RESET SESSION
      -- AUTHORIZATION` restores both users from it.
      sessAuthenticatedUser :: !(Maybe Ident)
    , -- The current session_user, changed by `SET SESSION AUTHORIZATION`.
      sessSessionUser :: !(Maybe Ident)
    , sessAutocommit :: !Bool
    , sessVariables :: !(Map Text SessionValue)
    , sessForeignKeyChecks :: !Bool
    , -- The configured start value, restored by RESET foreign_keys and RESET ALL.
      sessInitialForeignKeyChecks :: !Bool
    , sessTxNamespacePrefix :: !(Maybe [Ident])
    , sessTxSearchPath :: !(Maybe [SearchPathEntry])
    , sessTxSessionUser :: !(Maybe (Maybe Ident))
    , sessTxCurrentUser :: !(Maybe (Maybe Ident))
    , sessTxVariables :: !(Map Text SessionValue)
    , sessTempInNamespace :: !Bool
    , sessPermViewIgnoresTemp :: !Bool
    , sessViewsBoundToRelations :: !Bool
    }
    deriving (Eq, Show)

sessionFromInit :: SessionInit -> SessionState
sessionFromInit SessionInit {..} =
    SessionState
        { sessNamespacePrefix = sessionNamespacePrefix
        , sessSearchPath = sessionSearchPath
        , sessCurrentUser = sessionCurrentUser
        , sessAuthenticatedUser = sessionCurrentUser
        , sessSessionUser = sessionCurrentUser
        , sessAutocommit = sessionAutocommit
        , sessVariables = Map.map SessionText sessionVariables
        , sessForeignKeyChecks = sessionForeignKeyChecks
        , sessInitialForeignKeyChecks = sessionForeignKeyChecks
        , sessTxNamespacePrefix = Nothing
        , sessTxSearchPath = Nothing
        , sessTxSessionUser = Nothing
        , sessTxCurrentUser = Nothing
        , sessTxVariables = Map.empty
        , sessTempInNamespace = sessionTempInNamespace
        , sessPermViewIgnoresTemp = sessionPermViewIgnoresTemp
        , sessViewsBoundToRelations = sessionViewsBoundToRelations
        }

effectiveNamespacePrefix :: SessionState -> [Ident]
effectiveNamespacePrefix state =
    fromMaybe (sessNamespacePrefix state) (sessTxNamespacePrefix state)

effectiveCurrentUser :: SessionState -> Maybe Ident
effectiveCurrentUser state =
    fromMaybe (sessCurrentUser state) (sessTxCurrentUser state)

effectiveSessionUser :: SessionState -> Maybe Ident
effectiveSessionUser state =
    fromMaybe (sessSessionUser state) (sessTxSessionUser state)

-- | Resolve symbolic search path entries against the current user.
effectiveSearchPath :: SessionState -> [Name]
effectiveSearchPath state =
    case fromMaybe (sessSearchPath state) (sessTxSearchPath state) of
        [] -> []
        entries -> fromMaybe [] (traverse resolveEntry entries)
    where
        resolveEntry (SearchPathName name) = Just name
        resolveEntry SearchPathCurrentUser =
            Name . pure <$> effectiveCurrentUser state

clearTransactionSession :: SessionState -> SessionState
clearTransactionSession state =
    state
        { sessTxNamespacePrefix = Nothing
        , sessTxSearchPath = Nothing
        , sessTxSessionUser = Nothing
        , sessTxCurrentUser = Nothing
        , sessTxVariables = Map.empty
        }

data Savepoint = Savepoint
    { savepointName :: !Ident
    , savepointCatalog :: !Catalog
    , savepointSession :: !SessionState
    , savepointStatement :: !StatementId
    }
    deriving (Eq, Show)

data TxFrame = TxFrame
    { frameId :: !Int
    , frameExplicit :: !Bool
    , -- Nested transactions increment this counter; a commit only closes the
      -- outermost frame when the counter reaches zero.
      frameNesting :: !Int
    , frameBegin :: !(Maybe StatementId)
    , frameCatalog :: !Catalog
    , frameSession :: !SessionState
    , frameSavepoints :: ![Savepoint]
    }
    deriving (Eq, Show)

data ScanState = ScanState
    { scanCatalog :: !Catalog
    , scanSession :: !SessionState
    , scanTransactions :: ![TxFrame]
    , scanNextFrameId :: !Int
    , scanLastTransactionExit :: !(Maybe StatementId)
    , scanLastBarriers :: !(Map BarrierKey StatementId)
    , scanTouchers :: !(Map BarrierKey (Set StatementId))
    , scanSchemaTouchersByLast :: !(Map Ident (Map Name (Set StatementId)))
    , scanSchemaTouchersEmpty :: !(Set StatementId)
    , scanPriorStatements :: !(Set StatementId)
    }

initialScanState :: SessionInit -> ScanState
initialScanState initSession =
    let session = sessionFromInit initSession
        state =
            ScanState
                { scanCatalog = emptyCatalog
                , scanSession = session
                , scanTransactions = []
                , scanNextFrameId = 1
                , scanLastTransactionExit = Nothing
                , scanLastBarriers = Map.empty
                , scanTouchers = Map.empty
                , scanSchemaTouchersByLast = Map.empty
                , scanSchemaTouchersEmpty = Set.empty
                , scanPriorStatements = Set.empty
                }
     in if sessAutocommit session
            then state
            else
                state
                    { scanTransactions =
                        [ TxFrame
                            { frameId = 0
                            , frameExplicit = False
                            , frameNesting = 1
                            , frameBegin = Nothing
                            , frameCatalog = emptyCatalog
                            , frameSession = session
                            , frameSavepoints = []
                            }
                        ]
                    }

-- Evaluation accumulator
data Eval = Eval
    { evalDeps :: !(Set StatementId)
    , evalTouches :: !(Set BarrierKey)
    , evalBarriers :: ![ScopedBarrier]
    , evalCatalog :: !Catalog
    , evalSession :: !SessionState
    , evalVisitedTriggers :: !(Set TriggerKey)
    , evalVisitedWrites :: !(Set (RelationKey, WriteSignature))
    , evalFrameId :: !(Maybe Int)
    , evalTransactionActive :: !Bool
    , -- Used only for SQLite foreign-key PRAGMAs.
      evalTransactionBoundary :: Maybe StatementId
    , evalSessionTouched :: !Bool
    }

initialEval :: StatementId -> ScanState -> Eval
initialEval _ state =
    Eval
        { evalDeps = Set.empty
        , evalTouches = Set.empty
        , evalBarriers = []
        , evalCatalog = scanCatalog state
        , evalSession = scanSession state
        , evalVisitedTriggers = Set.empty
        , evalVisitedWrites = Set.empty
        , evalFrameId = frameId <$> listToMaybe (scanTransactions state)
        , evalTransactionActive = active
        , evalTransactionBoundary = if active then transactionOpener state else scanLastTransactionExit state
        , evalSessionTouched = False
        }
    where
        active = transactionActive state

transactionOpener :: ScanState -> Maybe StatementId
transactionOpener state = do
    frame <- listToMaybe (scanTransactions state)
    case frameBegin frame of
        Just begin -> Just begin
        Nothing -> savepointStatement <$> listToMaybe (reverse (frameSavepoints frame))

transactionActive :: ScanState -> Bool
transactionActive state =
    any (\frame -> frameExplicit frame || not (null (frameSavepoints frame))) (scanTransactions state)
        || not (sessAutocommit (scanSession state))

addDeps :: Set StatementId -> Eval -> Eval
addDeps deps eval
    | Set.null deps = eval
    | Set.null (evalDeps eval) = eval {evalDeps = deps}
    | otherwise = eval {evalDeps = Set.union deps (evalDeps eval)}

addTouch :: BarrierKey -> Eval -> Eval
addTouch key eval = eval {evalTouches = Set.insert key (evalTouches eval)}

addTouches :: [BarrierKey] -> Eval -> Eval
addTouches keys eval =
    eval {evalTouches = Foldable.foldl' (flip Set.insert) (evalTouches eval) keys}

addBarrier :: ScopedBarrier -> Eval -> Eval
addBarrier scoped eval = eval {evalBarriers = scoped : evalBarriers eval}

-- | Lens for the catalog component of an evaluation accumulator.
evalCatalogL :: Lens' Eval Catalog
evalCatalogL = lens evalCatalog (\e c -> e {evalCatalog = c})

-- | Lens for session state inside an evaluation state.
evalSessionL :: Lens' Eval SessionState
evalSessionL = lens evalSession (\e s -> e {evalSession = s})

-- | Lens for dependencies inside an evaluation state.
evalDepsL :: Lens' Eval (Set StatementId)
evalDepsL = lens evalDeps (\e d -> e {evalDeps = d})

-- | Lens for barrier touches inside an evaluation state.
evalTouchesL :: Lens' Eval (Set BarrierKey)
evalTouchesL = lens evalTouches (\e t -> e {evalTouches = t})

-- | Lens for scoped barriers inside an evaluation state.
evalBarriersL :: Lens' Eval [ScopedBarrier]
evalBarriersL = lens evalBarriers (\e b -> e {evalBarriers = b})

-- | Modify the catalog component of an evaluation accumulator.
modifyCatalog :: (Catalog -> Catalog) -> Eval -> Eval
modifyCatalog f eval = eval & evalCatalogL %~ f

-- | Modify the session component of an evaluation accumulator.
modifySession :: (SessionState -> SessionState) -> Eval -> Eval
modifySession f eval = eval & evalSessionL %~ f

-- | Modify a relation state within an evaluation state if present.
modifyEvalRelation :: RelationKey -> (RelationState -> RelationState) -> Eval -> Eval
modifyEvalRelation key f = modifyCatalog (modifyRelation key f)

ambiguousMutation :: Eval -> Eval
ambiguousMutation =
    addBarrier
        (ScopedBarrier GlobalScope UnknownStatementBarrier (Just "ambiguous catalog mutation target"))

-- Resolution may union possible read sources, but a mutation cannot apply to
-- every possible target. Explicit multi-target SQL emits one operation each.
mutateResolved :: (Eval -> key -> Eval) -> Eval -> [key] -> Eval
mutateResolved apply eval keys =
    case keys of
        _ : _ : _ -> ambiguousMutation eval
        _ -> Foldable.foldl' apply eval keys

evalToState :: Eval -> ScanState -> ScanState
evalToState eval state =
    state
        { scanCatalog = evalCatalog eval
        , scanSession = evalSession eval
        }
