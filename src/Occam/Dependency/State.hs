{-# LANGUAGE DeriveGeneric #-}

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
    -- | State for one index definition, its owning relation, and key columns.
    IndexState (..),
    -- | State for a trigger definition, timing, events, and body.
    TriggerState (..),
    -- | State for sequence definition and write history, including non-transactional writes.
    SequenceState (..),
    -- | State for a relation-owned constraint and its normalized definition.
    ConstraintState (..),
    -- | Definition and last-rollback markers for a type or schema.
    DefinitionState (..),
    -- | Catalog maps and derived indexes used during dependency scanning.
    Catalog (..),
    -- | Catalog with all object maps empty and derived indexes initialized.
    emptyCatalog,
    -- | Create an empty relation-state record for the supplied relation kind.
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

    -- | Look up a relation by its canonical key.
    lookupRelation,
    -- | Insert or replace a relation while maintaining derived catalog indexes.
    insertRelation,
    -- | Delete a relation entry and update its derived name/child indexes.
    deleteRelation,
    -- | Delete an index from the catalog map.
    deleteIndex,
    -- | Delete a trigger from the catalog map.
    deleteTrigger,
    -- | Delete a sequence from the catalog map.
    deleteSequence,
    -- | Delete a constraint and remove its relation/FK references.
    deleteConstraint,
    -- | Delete a relation and all indexes, triggers, and constraints it owns.
    clearRelationObjects,
    -- State modification helpers

    modifyCatalog,
    modifySession,
    modifyRelation,
    modifyEvalRelation,
    -- Write signatures and name utilities

    -- | Signature used to coalesce equivalent writes to one relation.
    WriteSignature (..),
    -- | Normalize a write kind and its target columns into a write signature.
    writeSignature,
    -- | Test whether one qualified name is a suffix of another.
    nameSuffix,
    -- | Return the final identifier component, if present.
    nameLast,
    -- | Rebuild the foreign-key lookup map from stored constraint states.
    rebuildForeignKeys,
    -- | Rebuild the set of relations that have partition or inheritance parents.
    rebuildChildRelations,
    -- | Rebuild the non-temporary relation index keyed by final identifier.
    rebuildRelationsByLast,
) where

import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
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

-- Session and transaction state

-- | Session configuration with transaction-local overrides and runtime switches.
data SessionState = SessionState
    { sessNamespacePrefix :: ![Ident]
    , sessSearchPath :: ![SearchPathEntry]
    , sessCurrentUser :: !(Maybe Ident)
    , sessAuthenticatedUser :: !(Maybe Ident)
    -- ^ User that originally authenticated the connection.
    , sessSessionUser :: !(Maybe Ident)
    -- ^ SQL session_user, changed by SET SESSION AUTHORIZATION.
    , sessAutocommit :: !Bool
    , sessVariables :: !(Map Text SessionValue)
    , sessForeignKeyChecks :: !Bool
    , sessInitialForeignKeyChecks :: !Bool
    -- ^ Configured value restored by RESET foreign_keys and RESET ALL.
    , sessTxNamespacePrefix :: !(Maybe [Ident])
    -- ^ Transaction-local override of the namespace prefix.
    , sessTxSearchPath :: !(Maybe [SearchPathEntry])
    -- ^ Transaction-local override of the search path.
    , sessTxSessionUser :: !(Maybe (Maybe Ident))
    -- ^ Outer Nothing inherits; Just Nothing explicitly clears.
    , sessTxCurrentUser :: !(Maybe (Maybe Ident))
    -- ^ Outer Nothing inherits; Just Nothing explicitly clears.
    , sessTxVariables :: !(Map Text SessionValue)
    -- ^ Transaction-local setting overrides.
    , sessTempInNamespace :: !Bool
    , sessPermViewIgnoresTemp :: !Bool
    , sessViewsBoundToRelations :: !Bool
    }
    deriving (Eq, Show)

-- | Initialize session state and its authenticated/session user from initialization options.
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

-- | Current namespace prefix, preferring a transaction-local override.
effectiveNamespacePrefix :: SessionState -> [Ident]
effectiveNamespacePrefix state =
    fromMaybe (sessNamespacePrefix state) (sessTxNamespacePrefix state)

-- | Current user after applying any transaction-local override.
effectiveCurrentUser :: SessionState -> Maybe Ident
effectiveCurrentUser state =
    fromMaybe (sessCurrentUser state) (sessTxCurrentUser state)

-- | Session user after applying any transaction-local override.
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

-- | Clear all transaction-local session overrides while preserving session values.
clearTransactionSession :: SessionState -> SessionState
clearTransactionSession state =
    state
        { sessTxNamespacePrefix = Nothing
        , sessTxSearchPath = Nothing
        , sessTxSessionUser = Nothing
        , sessTxCurrentUser = Nothing
        , sessTxVariables = Map.empty
        }

-- | Catalog and session snapshots made when a savepoint is created.
data Savepoint = Savepoint
    { savepointName :: !Ident
    , savepointCatalog :: !Catalog
    , savepointSession :: !SessionState
    , savepointStatement :: !StatementId
    }
    deriving (Eq, Show)

-- | Transaction snapshot plus explicit/nested state and its savepoint stack.
data TxFrame = TxFrame
    { frameId :: !Int
    , frameExplicit :: !Bool
    , frameNesting :: !Int
    -- ^ Nested begin count; commit closes only when it reaches zero.
    , frameBegin :: !(Maybe StatementId)
    , frameCatalog :: !Catalog
    , frameSession :: !SessionState
    , frameSavepoints :: ![Savepoint]
    }
    deriving (Eq, Show)

-- | Mutable scan-wide catalog/session state and dependency-barrier ordering indexes.
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

-- | Start a scan with an empty catalog and session settings from initialization.
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

-- | Per-statement dependency result and working copies of catalog/session state.
data Eval = Eval
    { evalDeps :: !(Set StatementId)
    , evalTouches :: !(Set BarrierKey)
    , evalBarriers :: ![ScopedBarrier]
    , evalCatalog :: !Catalog
    , evalSession :: !SessionState
    , evalSessionTouched :: !Bool
    , evalVisitedTriggers :: !(Set TriggerKey)
    , evalVisitedWrites :: !(Set (RelationKey, WriteSignature))
    , evalFrameId :: !(Maybe Int)
    , evalTransactionActive :: !Bool
    , evalTransactionBoundary :: Maybe StatementId
    -- ^ Transaction boundary used for SQLite foreign-key PRAGMA semantics.
    }
    deriving (Generic)

-- | Initialize an empty evaluation accumulator from the current scan snapshot.
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

-- | Return the statement that opened the active transaction or latest savepoint.
transactionOpener :: ScanState -> Maybe StatementId
transactionOpener state = do
    frame <- listToMaybe (scanTransactions state)
    case frameBegin frame of
        Just begin -> Just begin
        Nothing -> savepointStatement <$> listToMaybe (reverse (frameSavepoints frame))

-- | True when an explicit/savepoint frame exists or autocommit is disabled.
transactionActive :: ScanState -> Bool
transactionActive state =
    any (\frame -> frameExplicit frame || not (null (frameSavepoints frame))) (scanTransactions state)
        || not (sessAutocommit (scanSession state))

-- | Union statement dependencies into the evaluation accumulator.
addDeps :: Set StatementId -> Eval -> Eval
addDeps deps eval
    | Set.null deps = eval
    | Set.null (evalDeps eval) = eval {evalDeps = deps}
    | otherwise = eval {evalDeps = Set.union deps (evalDeps eval)}

-- | Record one barrier key touched by the current statement.
addTouch :: BarrierKey -> Eval -> Eval
addTouch key eval = eval {evalTouches = Set.insert key (evalTouches eval)}

-- | Record several barrier keys touched by the current statement.
addTouches :: [BarrierKey] -> Eval -> Eval
addTouches keys eval =
    eval {evalTouches = Foldable.foldl' (flip Set.insert) (evalTouches eval) keys}

-- | Add one scoped barrier to the evaluation result.
addBarrier :: ScopedBarrier -> Eval -> Eval
addBarrier scoped eval = eval {evalBarriers = scoped : evalBarriers eval}

modifyRelation :: RelationKey -> (RelationState -> RelationState) -> Catalog -> Catalog
modifyRelation key f catalog =
    catalog
        { catalogRelations =
            Map.adjust f key (catalogRelations catalog)
        }

-- | Modify the catalog component of an evaluation accumulator.
modifyCatalog :: (Catalog -> Catalog) -> Eval -> Eval
modifyCatalog f eval = eval {evalCatalog = f (evalCatalog eval)}

-- | Modify the session component of an evaluation accumulator.
modifySession :: (SessionState -> SessionState) -> Eval -> Eval
modifySession f eval = eval {evalSession = f (evalSession eval)}

-- | Modify a relation state within an evaluation state if present.
modifyEvalRelation :: RelationKey -> (RelationState -> RelationState) -> Eval -> Eval
modifyEvalRelation key f = modifyCatalog (modifyRelation key f)

-- | Add a global unknown-mutation barrier for an ambiguous catalog target.
ambiguousMutation :: Eval -> Eval
ambiguousMutation =
    addBarrier
        (ScopedBarrier GlobalScope UnknownStatementBarrier (Just "ambiguous catalog mutation target"))

{- | Apply an update only when resolution yields at most one possible target.
Multiple candidates create a conservative barrier; callers model explicit
multi-target statements separately.
-}
mutateResolved :: (Eval -> key -> Eval) -> Eval -> [key] -> Eval
mutateResolved apply eval keys =
    case keys of
        _ : _ : _ -> ambiguousMutation eval
        _ -> Foldable.foldl' apply eval keys

-- | Commit the evaluation's catalog and session values back into scan state.
evalToState :: Eval -> ScanState -> ScanState
evalToState eval state =
    state
        { scanCatalog = evalCatalog eval
        , scanSession = evalSession eval
        }
