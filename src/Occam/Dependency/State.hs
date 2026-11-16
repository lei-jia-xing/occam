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
    -- Write signatures and name utilities
    WriteSignature (..),
    writeSignature,
    nameSuffix,
    nameLast,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Occam.Core (
    BarrierReason (..),
    BarrierScope (..),
    Constraint,
    DeleteScope,
    Ident,
    Name (..),
    NameRef,
    OnCommitAction,
    ReadFact,
    RelationKind,
    ScopedBarrier (..),
    SearchPathEntry (..),
    SessionInit (..),
    SessionValue (..),
    StatementId,
    TriggerBody,
    TriggerEvent,
    TriggerTiming,
    WriteKind (..),
 )

-- Canonical catalog keys
data RelationKey = RelationKey
    { relationKeyTemp :: !Bool
    , relationKeyName :: !Name
    }
    deriving (Eq, Ord, Show)

data IndexKey
    = NamedIndexKey !Name
    | TableIndexKey !RelationKey !Ident
    deriving (Eq, Ord, Show)

data TriggerKey
    = NamedTriggerKey !Name
    | TableTriggerKey !RelationKey !Ident
    deriving (Eq, Ord, Show)

data SequenceKey = SequenceKey
    { sequenceKeyTemp :: !Bool
    , sequenceKeyName :: !Name
    }
    deriving (Eq, Ord, Show)

data ConstraintIdentity
    = NamedConstraint !Ident
    | UnnamedConstraint !StatementId !Int
    deriving (Eq, Ord, Show)

data ConstraintKey = ConstraintKey
    { constraintKeyTable :: !RelationKey
    , constraintKeyIdentity :: !ConstraintIdentity
    }
    deriving (Eq, Ord, Show)

data ObjectKey
    = BarrierRelationKey !RelationKey
    | BarrierIndexKey !IndexKey
    | BarrierTriggerKey !TriggerKey
    | BarrierSequenceKey !SequenceKey
    | BarrierTypeKey !Name
    | BarrierSchemaKey !Name
    | BarrierConstraintKey !ConstraintKey
    deriving (Eq, Ord, Show)

data BarrierKey
    = GlobalBarrierKey
    | SessionBarrierKey
    | TransactionBarrierKey
    | RelationBarrierKey !RelationKey
    | SchemaBarrierKey !Name
    | ObjectBarrierKey !ObjectKey
    deriving (Eq, Ord, Show)

-- Catalog state
data RelationState = RelationState
    { relKind :: !RelationKind
    , relDefinition :: !(Maybe StatementId)
    , relColumns :: !(Set Ident)
    , relDataWrites :: !(Set StatementId)
    , relColumnWrites :: !(Map Ident (Set StatementId))
    , relSchemaWrites :: !(Set StatementId)
    , relRestoredBy :: !(Maybe StatementId)
    , relSources :: !(Set RelationKey)
    , relUnresolvedSources :: !(Set NameRef)
    , -- The reads that defined a view or table source, retaining the columns
      -- used so column drops can distinguish dependent from unaffected views.
      relSourceReads :: ![ReadFact]
    , relOnCommit :: !(Maybe OnCommitAction)
    , relOnCommitFrameId :: !(Maybe Int)
    , relTransactional :: !Bool
    , relIndexes :: !(Set IndexKey)
    , relTriggers :: !(Set TriggerKey)
    , relConstraints :: !(Set ConstraintKey)
    }
    deriving (Eq, Show)

data IndexState = IndexState
    { indexDefinition :: !(Maybe StatementId)
    , indexStateTable :: !(Maybe RelationKey)
    , indexStateColumns :: ![Ident]
    , indexRestoredBy :: !(Maybe StatementId)
    }
    deriving (Eq, Show)

data TriggerState = TriggerState
    { triggerDefinition :: !(Maybe StatementId)
    , triggerStateTable :: !(Maybe RelationKey)
    , triggerStateTiming :: !TriggerTiming
    , triggerStateEvents :: ![TriggerEvent]
    , triggerStateWhenReads :: ![ReadFact]
    , triggerStateBody :: !TriggerBody
    , triggerRestoredBy :: !(Maybe StatementId)
    }
    deriving (Eq, Show)

data SequenceState = SequenceState
    { sequenceDefinition :: !(Maybe StatementId)
    , sequenceWrites :: !(Set StatementId)
    , sequenceRestoredBy :: !(Maybe StatementId)
    , sequenceStateTransactional :: !Bool
    }
    deriving (Eq, Show)

data ConstraintState = ConstraintState
    { constraintStateTable :: !RelationKey
    , constraintDefinition :: !(Maybe StatementId)
    , constraintValue :: !Constraint
    , constraintRestoredBy :: !(Maybe StatementId)
    }
    deriving (Eq, Show)

data DefinitionState = DefinitionState
    { definitionStatement :: !(Maybe StatementId)
    , definitionRestoredBy :: !(Maybe StatementId)
    }
    deriving (Eq, Show)

data Catalog = Catalog
    { catalogRelations :: !(Map RelationKey RelationState)
    , catalogIndexes :: !(Map IndexKey IndexState)
    , catalogTriggers :: !(Map TriggerKey TriggerState)
    , catalogSequences :: !(Map SequenceKey SequenceState)
    , catalogConstraints :: !(Map ConstraintKey ConstraintState)
    , catalogTypes :: !(Map Name DefinitionState)
    , catalogSchemas :: !(Map Name DefinitionState)
    , catalogNextUnnamedConstraint :: !Int
    {- ^ Monotonic serial used to give unnamed constraints distinct identities.
    A per-statement index is not enough: one statement can add several
    constraints through separate @ADD COLUMN@ changes.
    -}
    }
    deriving (Eq, Show)

emptyCatalog :: Catalog
emptyCatalog =
    Catalog
        { catalogRelations = Map.empty
        , catalogIndexes = Map.empty
        , catalogTriggers = Map.empty
        , catalogSequences = Map.empty
        , catalogConstraints = Map.empty
        , catalogTypes = Map.empty
        , catalogSchemas = Map.empty
        , catalogNextUnnamedConstraint = 0
        }

newRelation :: RelationKind -> RelationState
newRelation kind =
    RelationState
        { relKind = kind
        , relDefinition = Nothing
        , relColumns = Set.empty
        , relDataWrites = Set.empty
        , relColumnWrites = Map.empty
        , relSchemaWrites = Set.empty
        , relRestoredBy = Nothing
        , relSources = Set.empty
        , relUnresolvedSources = Set.empty
        , relSourceReads = []
        , relOnCommit = Nothing
        , relOnCommitFrameId = Nothing
        , relTransactional = True
        , relIndexes = Set.empty
        , relTriggers = Set.empty
        , relConstraints = Set.empty
        }

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

{- | Resolve the symbolic search path against the current user. An empty list
means "unknown" and makes unqualified resolution conservative, as does an
unresolved @$user@ entry.
-}
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
    , scanLastBarriers :: !(Map BarrierKey StatementId)
    , scanTouchers :: !(Map BarrierKey (Set StatementId))
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
                , scanLastBarriers = Map.empty
                , scanTouchers = Map.empty
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
        , evalTransactionActive =
            any frameExplicit (scanTransactions state)
                || not (sessAutocommit (scanSession state))
        , evalSessionTouched = False
        }

addDeps :: Set StatementId -> Eval -> Eval
addDeps deps eval = eval {evalDeps = Set.union deps (evalDeps eval)}

addTouch :: BarrierKey -> Eval -> Eval
addTouch key eval = eval {evalTouches = Set.insert key (evalTouches eval)}

addTouches :: [BarrierKey] -> Eval -> Eval
addTouches keys eval = eval {evalTouches = Set.union (Set.fromList keys) (evalTouches eval)}

addBarrier :: ScopedBarrier -> Eval -> Eval
addBarrier scoped eval = eval {evalBarriers = scoped : evalBarriers eval}

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
        _ -> foldl' apply eval keys

evalToState :: Eval -> ScanState -> ScanState
evalToState eval state =
    state
        { scanCatalog = evalCatalog eval
        , scanSession = evalSession eval
        }

-- Pure catalog operations
lookupRelation :: Catalog -> RelationKey -> Maybe RelationState
lookupRelation catalog key = Map.lookup key (catalogRelations catalog)

insertRelation :: RelationKey -> RelationState -> Catalog -> Catalog
insertRelation key relation catalog =
    catalog {catalogRelations = Map.insert key relation (catalogRelations catalog)}

deleteRelation :: RelationKey -> Catalog -> Catalog
deleteRelation key catalog =
    catalog {catalogRelations = Map.delete key (catalogRelations catalog)}

deleteIndex :: IndexKey -> Catalog -> Catalog
deleteIndex key catalog =
    catalog {catalogIndexes = Map.delete key (catalogIndexes catalog)}

deleteTrigger :: TriggerKey -> Catalog -> Catalog
deleteTrigger key catalog =
    catalog {catalogTriggers = Map.delete key (catalogTriggers catalog)}

deleteSequence :: SequenceKey -> Catalog -> Catalog
deleteSequence key catalog =
    catalog {catalogSequences = Map.delete key (catalogSequences catalog)}

deleteConstraint :: ConstraintKey -> Catalog -> Catalog
deleteConstraint key catalog =
    catalog
        { catalogConstraints = Map.delete key (catalogConstraints catalog)
        , catalogRelations =
            Map.adjust
                (\relation -> relation {relConstraints = Set.delete key (relConstraints relation)})
                (constraintKeyTable key)
                (catalogRelations catalog)
        }

clearRelationObjects :: RelationKey -> Catalog -> Catalog
clearRelationObjects key catalog =
    case lookupRelation catalog key of
        Nothing -> catalog
        Just relation ->
            let withoutIndexes =
                    foldl' (flip deleteIndex) catalog (Set.toList (relIndexes relation))
                withoutTriggers =
                    foldl' (flip deleteTrigger) withoutIndexes (Set.toList (relTriggers relation))
                withoutConstraints =
                    foldl' (flip deleteConstraint) withoutTriggers (Set.toList (relConstraints relation))
             in deleteRelation key withoutConstraints

-- Write signatures
data WriteSignature
    = InsertSignature
    | UpdateSignature !(Set Ident)
    | DeleteSignature !DeleteScope
    | MergeSignature
    | TruncateSignature
    deriving (Eq, Ord, Show)

writeSignature :: WriteKind -> [Ident] -> WriteSignature
writeSignature kind columns =
    case kind of
        InsertWrite _ -> InsertSignature
        UpdateWrite -> UpdateSignature (Set.fromList columns)
        DeleteWrite scope -> DeleteSignature scope
        MergeWrite -> MergeSignature
        TruncateWrite {} -> TruncateSignature

-- Name utilities
nameSuffix :: Name -> Name -> Bool
nameSuffix (Name requested) (Name stored) =
    length requested <= length stored
        && drop (length stored - length requested) stored == requested

nameLast :: Name -> Maybe Ident
nameLast (Name parts) =
    case parts of
        [] -> Nothing
        xs -> Just (last xs)
