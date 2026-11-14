{-# LANGUAGE RecordWildCards #-}

module Occam.Dependency (
    DependencyGraph (..),
    emptyGraph,
    buildGraph,
    analyzeStatements,
) where

import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Occam.Core (
    AlterChange (..),
    AlterTable (..),
    BarrierReason (..),
    BarrierScope (..),
    BeginPolicy (..),
    CatalogOp (..),
    ColumnDef (..),
    ConflictAction (..),
    Constraint (..),
    ConstraintRef (..),
    ConstraintValidity (..),
    DataWrite (..),
    DeleteScope (..),
    DmlEvent (..),
    DropMode (..),
    DropObject (..),
    DropOp (..),
    ForeignKey (..),
    Ident (..),
    IndexDef (..),
    IndexRef (..),
    IndexRole (..),
    IndexSchemaPlacement (..),
    Name (..),
    NameRef (..),
    OnCommitAction (..),
    ReadFact (..),
    ReadMode (..),
    ReferentialAction (..),
    RefreshOp (..),
    RelationKind (..),
    RenameOp (..),
    SchemaDef (..),
    ScopedBarrier (..),
    SearchPathEntry (..),
    SearchPathValue (..),
    SemanticFacts (..),
    SequenceDef (..),
    SequenceWrite (..),
    SessionEffect (..),
    SessionInit (..),
    SessionScope (..),
    SessionValue (..),
    Statement (..),
    StatementId (..),
    TableDef (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEffect (..),
    TriggerEvent (..),
    TriggerRef (..),
    TriggerTiming (..),
    TruncateBehavior (..),
    TruncateIdentity (..),
    TxEffect (..),
    TypeDef (..),
    ViewDef (..),
    WriteKind (..),
    nameText,
 )
import Occam.Core qualified as Core
import Occam.Facts (StatementFacts (..), StatementUnit (..))

-- ----------------------------------------------------------------------------
-- Public graph
-- ----------------------------------------------------------------------------

data DependencyGraph = DependencyGraph
    { graphStmts :: !(Set StatementId)
    , graphFacts :: !(Map StatementId SemanticFacts)
    , graphEdges :: !(Map StatementId (Set StatementId))
    , graphBarrierActions :: !Int
    , graphBarrierStatements :: !Int
    , graphBarrierEdges :: !Int
    , graphEdgeCount :: !Int
    }
    deriving (Eq, Show)

emptyGraph :: DependencyGraph
emptyGraph =
    DependencyGraph
        { graphStmts = Set.empty
        , graphFacts = Map.empty
        , graphEdges = Map.empty
        , graphBarrierActions = 0
        , graphBarrierStatements = 0
        , graphBarrierEdges = 0
        , graphEdgeCount = 0
        }

-- ----------------------------------------------------------------------------
-- Canonical catalog keys
-- ----------------------------------------------------------------------------

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

-- ----------------------------------------------------------------------------
-- Catalog state
-- ----------------------------------------------------------------------------

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

-- ----------------------------------------------------------------------------
-- Session and transaction state
-- ----------------------------------------------------------------------------

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

-- ----------------------------------------------------------------------------
-- Evaluation accumulator
-- ----------------------------------------------------------------------------

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

-- ----------------------------------------------------------------------------
-- Graph construction
-- ----------------------------------------------------------------------------

extractFacts :: StatementUnit -> SemanticFacts
extractFacts unit =
    maybe
        SemanticFacts
            { factOps = []
            , factSession = []
            , factPreTx = []
            , factPostTx = []
            , factBarriers =
                [ ScopedBarrier
                    GlobalScope
                    (if isJust (unitParseError unit) then SyntaxErrorBarrier else UnknownStatementBarrier)
                    Nothing
                ]
            }
        factSemantics
        (unitFacts unit)

analyzeStatements :: SessionInit -> [StatementUnit] -> DependencyGraph
analyzeStatements initialSession units =
    buildGraph
        initialSession
        [ (stmtId (unitStatement unit), extractFacts unit)
        | unit <- units
        ]

buildGraph :: SessionInit -> [(StatementId, SemanticFacts)] -> DependencyGraph
buildGraph initialSession stmtFacts =
    let (_, edges, barrierEdgeCount, barrierStmtCount, barrierActionCount) =
            foldl' step (initialScanState initialSession, Map.empty, 0, 0, 0) stmtFacts
     in DependencyGraph
            { graphStmts = Set.fromList (map fst stmtFacts)
            , graphFacts = Map.fromList stmtFacts
            , graphEdges = edges
            , graphBarrierActions = barrierActionCount
            , graphBarrierStatements = barrierStmtCount
            , graphBarrierEdges = barrierEdgeCount
            , graphEdgeCount = sum (map Set.size (Map.elems edges))
            }

step ::
    (ScanState, Map StatementId (Set StatementId), Int, Int, Int) ->
    (StatementId, SemanticFacts) ->
    (ScanState, Map StatementId (Set StatementId), Int, Int, Int)
step (priorState, edges, barrierEdgeCount, barrierStmtCount, barrierActionCount) (sid, facts) =
    let
        (state1, _) = ensureFrame priorState
        state2 = applyTxEffects sid (factPreTx facts) state1
        state3 = ensureFrameForOps state2
        eval0 = initialEval sid state3
        eval1 = foldl' (processOp sid) eval0 (factOps facts)
        eval1' = foldl' (flip addBarrier) eval1 (factBarriers facts)
        (eval2, sessionTouched) = applySessionEffects sid (factSession facts) eval1'
        state4 = evalToState eval2 state3
        state5 = settleAutocommit sid state3 state4
        state6 = applyTxEffects sid (factPostTx facts) state5
        eval3 =
            eval2
                { evalCatalog = scanCatalog state6
                , evalSession = scanSession state6
                }
        (deps, state7, newBarrierEdges, hasBarrier, barrierActions) =
            finalizeStatement sid eval3 sessionTouched priorState state6
        state8 = finishStatement sid state7
        edges' = Map.insert sid deps edges
     in
        ( state8
        , edges'
        , barrierEdgeCount + newBarrierEdges
        , barrierStmtCount + if hasBarrier then 1 else 0
        , barrierActionCount + barrierActions
        )

-- ----------------------------------------------------------------------------
-- Name resolution
-- ----------------------------------------------------------------------------

nameSuffix :: Name -> Name -> Bool
nameSuffix (Name requested) (Name stored) =
    length requested <= length stored
        && drop (length stored - length requested) stored == requested

qualifyDefinitionName :: SessionState -> Name -> Bool -> Name
qualifyDefinitionName session name@(Name parts) temporary
    | temporary = name
    | length parts /= 1 = name
    | otherwise =
        case effectiveNamespacePrefix session of
            prefix@(_ : _) -> Name (prefix ++ parts)
            [] ->
                case effectiveSearchPath session of
                    Name searchParts : _ | not (null searchParts) -> Name (searchParts ++ parts)
                    _ -> name

qualifyObjectName :: SessionState -> Name -> Name
qualifyObjectName session name = qualifyDefinitionName session name False

qualifyReferenceName :: SessionState -> Name -> Name
qualifyReferenceName = qualifyObjectName

qualifyRenameName :: Name -> Name -> Name
qualifyRenameName (Name oldParts) newName@(Name newParts)
    | length oldParts > 1 && length newParts == 1 =
        Name (init oldParts ++ newParts)
    | otherwise = newName

relationFallbackKey :: SessionState -> NameRef -> RelationKey
relationFallbackKey session (NameRef name temporary)
    | temporary = RelationKey True name
    | length (nameParts name) > 1 = RelationKey False name
    | otherwise = RelationKey False (qualifyReferenceName session name)

sequenceFallbackKey :: SessionState -> NameRef -> SequenceKey
sequenceFallbackKey session (NameRef name temporary)
    | temporary = SequenceKey True name
    | length (nameParts name) > 1 = SequenceKey False name
    | otherwise = SequenceKey False (qualifyReferenceName session name)

resolveRelationKeys :: SessionState -> Catalog -> NameRef -> [RelationKey]
resolveRelationKeys session catalog (NameRef name temporary)
    | temporary = [RelationKey True name]
    | length (nameParts name) > 1 = [RelationKey False name]
    | Map.member temporaryKey relations = [temporaryKey]
    -- A namespace prefix is an exact current database, not an open search
    -- path: unqualified names must not leak into other databases.
    | not (null prefixKeys) = prefixKeys
    | null (effectiveSearchPath session) =
        case wildcardMatches of
            [] -> [fallbackKey]
            matches -> matches
    | otherwise =
        case firstExisting searchKeys of
            Just key -> [key]
            Nothing -> [fallbackKey]
    where
        relations = catalogRelations catalog
        temporaryKey = RelationKey True name
        fallbackKey = relationFallbackKey session (NameRef name temporary)
        prefixKeys =
            case effectiveNamespacePrefix session of
                [] -> []
                prefix -> [RelationKey False (Name (prefix ++ nameParts name))]
        searchKeys =
            prefixKeys
                ++ [ RelationKey False (Name (nameParts entry ++ nameParts name))
                   | entry <- effectiveSearchPath session
                   ]
                ++ [RelationKey False name]
        firstExisting = find (`Map.member` relations)
        wildcardMatches =
            [ key
            | key@(RelationKey False stored) <- Map.keys relations
            , nameSuffix name stored
            ]

resolveSequenceKeys :: SessionState -> Catalog -> NameRef -> [SequenceKey]
resolveSequenceKeys session catalog (NameRef name temporary)
    | temporary = [SequenceKey True name]
    | length (nameParts name) > 1 = [SequenceKey False name]
    | Map.member temporaryKey sequences = [temporaryKey]
    | not (null prefixKeys) = prefixKeys
    | null (effectiveSearchPath session) =
        case wildcardMatches of
            [] -> [fallbackKey]
            matches -> matches
    | otherwise =
        case firstExisting searchKeys of
            Just key -> [key]
            Nothing -> [fallbackKey]
    where
        sequences = catalogSequences catalog
        temporaryKey = SequenceKey True name
        fallbackKey = sequenceFallbackKey session (NameRef name temporary)
        prefixKeys =
            case effectiveNamespacePrefix session of
                [] -> []
                prefix -> [SequenceKey False (Name (prefix ++ nameParts name))]
        searchKeys =
            prefixKeys
                ++ [ SequenceKey False (Name (nameParts entry ++ nameParts name))
                   | entry <- effectiveSearchPath session
                   ]
                ++ [SequenceKey False name]
        firstExisting = find (`Map.member` sequences)
        wildcardMatches =
            [ key
            | key@(SequenceKey False stored) <- Map.keys sequences
            , nameSuffix name stored
            ]

resolveIndexKeys :: SessionState -> Catalog -> IndexRef -> [IndexKey]
resolveIndexKeys session catalog indexRef =
    case indexRef of
        NamedIndex name ->
            [ NamedIndexKey resolved
            | resolved <- resolveSchemaObjectName session (schemaIndexNames catalog) name
            ]
        TableIndex table ident ->
            [ TableIndexKey relationKey ident
            | relationKey <- resolveRelationKeys session catalog table
            ]

resolveTriggerKeys :: SessionState -> Catalog -> TriggerRef -> [TriggerKey]
resolveTriggerKeys session catalog triggerRef =
    case triggerRef of
        NamedTrigger name ->
            [ NamedTriggerKey resolved
            | resolved <- resolveSchemaObjectName session (schemaTriggerNames catalog) name
            ]
        TableTrigger table ident ->
            [ TableTriggerKey relationKey ident
            | relationKey <- resolveRelationKeys session catalog table
            ]

{- | Resolve a schema-level object name (index or schema-scoped trigger) using
the same namespace/search-path rules as relations.
-}
resolveSchemaObjectName :: SessionState -> [Name] -> Name -> [Name]
resolveSchemaObjectName session existing name@(Name parts)
    | length parts > 1 = [name]
    | not (null prefixKeys) = prefixKeys
    | null (effectiveSearchPath session) =
        case wildcardMatches of
            [] -> [fallback]
            matches -> matches
    | otherwise =
        case firstExisting searchKeys of
            Just matched -> [matched]
            Nothing -> [fallback]
    where
        fallback = qualifyObjectName session name
        prefixKeys =
            case effectiveNamespacePrefix session of
                [] -> []
                prefix -> [Name (prefix ++ parts)]
        searchKeys =
            prefixKeys
                ++ [ Name (nameParts entry ++ parts)
                   | entry <- effectiveSearchPath session
                   ]
                ++ [name]
        existingSet = Set.fromList existing
        firstExisting = find (`Set.member` existingSet)
        wildcardMatches =
            [ stored
            | stored <- Set.toList existingSet
            , nameSuffix name stored
            ]

schemaIndexNames :: Catalog -> [Name]
schemaIndexNames catalog =
    [ name
    | NamedIndexKey name <- Map.keys (catalogIndexes catalog)
    ]

schemaTriggerNames :: Catalog -> [Name]
schemaTriggerNames catalog =
    [ name
    | NamedTriggerKey name <- Map.keys (catalogTriggers catalog)
    ]

resolveConstraintKeys :: SessionState -> Catalog -> ConstraintRef -> [ConstraintKey]
resolveConstraintKeys session catalog (ConstraintRef table name) =
    [ ConstraintKey relationKey (NamedConstraint name)
    | relationKey <- resolveRelationKeys session catalog table
    ]

resolveObjectKeys :: SessionState -> Catalog -> Core.ObjectRef -> [ObjectKey]
resolveObjectKeys session catalog objectRef =
    case objectRef of
        Core.ObjectRelation relation ->
            map BarrierRelationKey (resolveRelationKeys session catalog relation)
        Core.ObjectIndex index ->
            map BarrierIndexKey (resolveIndexKeys session catalog index)
        Core.ObjectTrigger trigger ->
            map BarrierTriggerKey (resolveTriggerKeys session catalog trigger)
        Core.ObjectSequence sequenceRef ->
            map BarrierSequenceKey (resolveSequenceKeys session catalog sequenceRef)
        Core.ObjectType name -> [BarrierTypeKey name]
        Core.ObjectSchema name -> [BarrierSchemaKey name]
        Core.ObjectConstraint constraintRef ->
            map BarrierConstraintKey (resolveConstraintKeys session catalog constraintRef)

-- ----------------------------------------------------------------------------
-- Barrier keys and matching
-- ----------------------------------------------------------------------------

barrierKeyMatches :: BarrierKey -> BarrierKey -> Bool
barrierKeyMatches (SchemaBarrierKey left) (SchemaBarrierKey right) =
    nameSuffix left right || nameSuffix right left
barrierKeyMatches left right = left == right

relationSchemaBarrierKeys :: RelationKey -> [BarrierKey]
relationSchemaBarrierKeys (RelationKey _ name) =
    map SchemaBarrierKey (dedupe (prefixes ++ lastPrefix))
    where
        parts = nameParts name
        prefixes = [Name (take n parts) | n <- [1 .. length parts - 1]]
        lastPrefix =
            [ Name [last (init parts)]
            | length parts > 1
            ]

dedupe :: (Ord a) => [a] -> [a]
dedupe = Set.toList . Set.fromList

relationTouchKeys :: RelationKey -> [BarrierKey]
relationTouchKeys key =
    RelationBarrierKey key
        : ObjectBarrierKey (BarrierRelationKey key)
        : relationSchemaBarrierKeys key

indexTouchKeys :: IndexKey -> Maybe RelationKey -> [BarrierKey]
indexTouchKeys key table =
    ObjectBarrierKey (BarrierIndexKey key)
        : maybe [] relationTouchKeys table

triggerTouchKeys :: TriggerKey -> Maybe RelationKey -> [BarrierKey]
triggerTouchKeys key table =
    ObjectBarrierKey (BarrierTriggerKey key)
        : maybe [] relationTouchKeys table

sequenceTouchKeys :: SequenceKey -> [BarrierKey]
sequenceTouchKeys key = [ObjectBarrierKey (BarrierSequenceKey key)]

typeTouchKeys :: Name -> [BarrierKey]
typeTouchKeys name = [ObjectBarrierKey (BarrierTypeKey name)]

schemaTouchKeys :: Name -> [BarrierKey]
schemaTouchKeys name = [ObjectBarrierKey (BarrierSchemaKey name), SchemaBarrierKey name]

-- All namespace parts use the frontend's relation identifier rules.
namespaceDeps :: Catalog -> [Ident] -> Set StatementId
namespaceDeps catalog parts =
    Set.fromList
        [ sid
        | count <- [1 .. length parts]
        , Just state <- [Map.lookup (Name (take count parts)) (catalogSchemas catalog)]
        , sid <- catMaybes [definitionStatement state, definitionRestoredBy state]
        ]

schemaPrefixDeps :: Catalog -> Name -> Set StatementId
schemaPrefixDeps catalog (Name parts) =
    namespaceDeps catalog (take (length parts - 1) parts)

barrierKeysForScope :: SessionState -> Catalog -> BarrierScope -> [BarrierKey]
barrierKeysForScope session catalog scope =
    case scope of
        GlobalScope -> [GlobalBarrierKey]
        SessionStateScope -> [SessionBarrierKey]
        TransactionStateScope -> [TransactionBarrierKey]
        RelationScope relation ->
            map RelationBarrierKey (resolveRelationKeys session catalog relation)
        SchemaScope name -> [SchemaBarrierKey name]
        ObjectScope objectRef ->
            map ObjectBarrierKey (resolveObjectKeys session catalog objectRef)

lastBarrierDeps :: ScanState -> [BarrierKey] -> Set StatementId
lastBarrierDeps state keys =
    Set.fromList
        [ sid
        | key <- keys
        , Just sid <- [Map.lookup key (scanLastBarriers state)]
        ]

touchersForBarrier :: ScanState -> BarrierKey -> Set StatementId
touchersForBarrier state key =
    Set.unions
        [ touchers
        | (candidate, touchers) <- Map.toList (scanTouchers state)
        , barrierKeyMatches key candidate
        ]

-- ----------------------------------------------------------------------------
-- Catalog helpers
-- ----------------------------------------------------------------------------

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

relationDefinitionDeps :: Catalog -> RelationKey -> Set StatementId
relationDefinitionDeps catalog key =
    case lookupRelation catalog key of
        Nothing -> Set.empty
        Just relation ->
            Set.fromList
                ( catMaybes
                    [ relDefinition relation
                    , relRestoredBy relation
                    ]
                )
                `Set.union` relSchemaWrites relation

relationColumnWriteDeps ::
    SessionState -> Catalog -> RelationKey -> RelationState -> [Ident] -> Set StatementId
relationColumnWriteDeps session catalog key relation columns
    | null columns = Set.unions (Map.elems (relColumnWrites relation))
    | otherwise =
        Set.unions
            [ Map.findWithDefault Set.empty column (relColumnWrites relation)
            | column <- Set.toList (coupledWriteColumns session catalog key columns)
            ]

{- | Expand written columns through the constraints they participate in. A
write to one column of a composite primary key, unique constraint, foreign key,
or check constraint can affect rows selected through the other columns, so
column-level dependency tracking must not treat those columns as independent.
-}
coupledWriteColumns :: SessionState -> Catalog -> RelationKey -> [Ident] -> Set Ident
coupledWriteColumns session catalog key columns
    | null columns = Set.empty
    | otherwise =
        let written = Set.fromList columns
            relation = fromMaybe (newRelation TableRelation) (lookupRelation catalog key)
            localSets =
                [ constraintCouplingColumns (constraintValue state)
                | constraintKey <- Set.toList (relConstraints relation)
                , Just state <- [Map.lookup constraintKey (catalogConstraints catalog)]
                ]
                    ++ [ -- A self-referential foreign key links its child and
                       -- parent columns inside the same relation, so they must
                       -- expand together instead of as disjoint candidates.
                       foreignKeyChildColumns foreignKey
                        ++ referencedParentColumns session catalog foreignKey
                       | (_, state, foreignKey) <- allForeignKeys catalog
                       , constraintStateTable state == key
                       , key
                            `elem` resolveRelationKeys
                                session
                                catalog
                                (foreignKeyParent foreignKey)
                       ]
            parentSets =
                [ referencedParentColumns session catalog foreignKey
                | (_, state, foreignKey) <- allForeignKeys catalog
                , constraintStateTable state /= key
                , key
                    `elem` resolveRelationKeys
                        session
                        catalog
                        (foreignKeyParent foreignKey)
                ]
            candidates = map Set.fromList (localSets ++ parentSets)
            expand current =
                let next =
                        Set.unions
                            [ candidate
                            | candidate <- candidates
                            , not (Set.null (Set.intersection current candidate))
                            ]
                    expanded = Set.union current next
                 in if expanded == current then current else expand expanded
         in expand written

constraintCouplingColumns :: Constraint -> [Ident]
constraintCouplingColumns constraint =
    case constraint of
        PrimaryKeyConstraint _ columns -> columns
        UniqueConstraint _ columns -> columns
        ForeignKeyConstraint foreignKey -> foreignKeyChildColumns foreignKey
        CheckConstraint _ columns _ -> columns
        IndexConstraint _ columns _ -> columns
        UsingIndexConstraint {} -> []
        OtherConstraint _ -> []

{- | Columns referenced in the parent table. When the fact IR omits the parent
column list, the parent's primary key is referenced.
-}
referencedParentColumns :: SessionState -> Catalog -> ForeignKey -> [Ident]
referencedParentColumns session catalog foreignKey
    | not (null (foreignKeyParentColumns foreignKey)) =
        foreignKeyParentColumns foreignKey
    | otherwise =
        concat
            [ keyColumns
            | parentKey <-
                resolveRelationKeys
                    session
                    catalog
                    (foreignKeyParent foreignKey)
            , (_, state) <- Map.toList (catalogConstraints catalog)
            , constraintStateTable state == parentKey
            , PrimaryKeyConstraint _ keyColumns <- [constraintValue state]
            ]

relationIndexDeps :: Catalog -> RelationState -> Set StatementId
relationIndexDeps catalog relation =
    Set.unions
        [ Set.fromList (catMaybes [indexDefinition index, indexRestoredBy index])
        | key <- Set.toList (relIndexes relation)
        , Just index <- [Map.lookup key (catalogIndexes catalog)]
        ]

relationTriggerDeps :: Catalog -> RelationState -> Set StatementId
relationTriggerDeps catalog relation =
    Set.unions
        [ Set.fromList (catMaybes [triggerDefinition trigger, triggerRestoredBy trigger])
        | key <- Set.toList (relTriggers relation)
        , Just trigger <- [Map.lookup key (catalogTriggers catalog)]
        ]

relationReadDeps :: SessionState -> Catalog -> RelationKey -> ReadMode -> [Ident] -> Set StatementId
relationReadDeps session catalog key mode columns =
    go Set.empty key
    where
        go seen current
            | Set.member current seen = Set.empty
            | otherwise =
                case lookupRelation catalog current of
                    Nothing -> Set.empty
                    Just relation ->
                        let seen' = Set.insert current seen
                            base =
                                relationDefinitionDeps catalog current
                                    `Set.union` relationIndexDeps catalog relation
                            dataDeps =
                                case mode of
                                    ReadData ->
                                        relDataWrites relation
                                            `Set.union` relationColumnWriteDeps session catalog current relation columns
                                    ReadSchema -> Set.empty
                            -- Tables populated by CTAS and materialized views
                            -- are snapshots. Their creation/refresh statement
                            -- already depends on the source data it evaluated.
                            sourceDeps =
                                Set.unions
                                    [ go seen' source
                                    | relKind relation == ViewRelation
                                    , source <- Set.toList (relSources relation)
                                    ]
                            -- A source that did not exist when the view was defined
                            -- is resolved against the catalog at read time, so a
                            -- later definition is still followed.
                            unresolvedDeps =
                                Set.unions
                                    [ go seen' sourceKey
                                    | relKind relation == ViewRelation
                                    , reference <- Set.toList (relUnresolvedSources relation)
                                    , sourceKey <- resolveRelationKeys session catalog reference
                                    , Map.member sourceKey (catalogRelations catalog)
                                    ]
                         in base `Set.union` dataDeps `Set.union` sourceDeps `Set.union` unresolvedDeps

relationWriteDeps ::
    SessionState -> Catalog -> RelationKey -> WriteKind -> [Ident] -> Set StatementId
relationWriteDeps session catalog key kind columns =
    case lookupRelation catalog key of
        Nothing -> Set.empty
        Just relation ->
            let base =
                    relationDefinitionDeps catalog key
                        `Set.union` relationIndexDeps catalog relation
                        `Set.union` relationTriggerDeps catalog relation
                writes =
                    case kind of
                        TruncateWrite {} -> Set.empty
                        _ ->
                            relDataWrites relation
                                `Set.union` relationColumnWriteDeps session catalog key relation columns
                sourceDeps =
                    Set.unions
                        [ relationReadDeps session catalog source ReadData []
                        | relKind relation == ViewRelation
                        , source <- Set.toList (relSources relation)
                        ]
                unresolvedDeps =
                    Set.unions
                        [ relationReadDeps session catalog sourceKey ReadData []
                        | relKind relation == ViewRelation
                        , reference <- Set.toList (relUnresolvedSources relation)
                        , sourceKey <- resolveRelationKeys session catalog reference
                        , Map.member sourceKey (catalogRelations catalog)
                        ]
             in base `Set.union` writes `Set.union` sourceDeps `Set.union` unresolvedDeps

relationAllDeps :: SessionState -> Catalog -> RelationKey -> Set StatementId
relationAllDeps session catalog key =
    relationReadDeps session catalog key ReadData []
        `Set.union` relationWriteDeps session catalog key (DeleteWrite DeleteFilteredRows) []

applyWriteState :: StatementId -> WriteKind -> [Ident] -> RelationState -> RelationState
applyWriteState sid kind columns relation =
    case kind of
        InsertWrite _ ->
            relation {relDataWrites = Set.insert sid (relDataWrites relation)}
        MergeWrite ->
            relation {relDataWrites = Set.insert sid (relDataWrites relation)}
        DeleteWrite scope
            | scope == DeleteAllRows ->
                relation
                    { relDataWrites = Set.singleton sid
                    , relColumnWrites = Map.empty
                    }
            | otherwise ->
                relation {relDataWrites = Set.insert sid (relDataWrites relation)}
        UpdateWrite
            | null columns ->
                relation {relDataWrites = Set.insert sid (relDataWrites relation)}
            | otherwise ->
                relation
                    { relColumnWrites =
                        foldl'
                            (\current column -> Map.insertWith Set.union column (Set.singleton sid) current)
                            (relColumnWrites relation)
                            columns
                    }
        TruncateWrite {} ->
            relation
                { relDataWrites = Set.singleton sid
                , relColumnWrites = Map.empty
                }

allForeignKeys :: Catalog -> [(ConstraintKey, ConstraintState, ForeignKey)]
allForeignKeys catalog =
    [ (key, state, foreignKey)
    | (key, state) <- Map.toList (catalogConstraints catalog)
    , ForeignKeyConstraint foreignKey <- [constraintValue state]
    ]

constraintNameIdent :: Constraint -> Maybe Ident
constraintNameIdent constraint =
    case constraint of
        PrimaryKeyConstraint name _ -> name
        UniqueConstraint name _ -> name
        ForeignKeyConstraint foreignKey -> foreignKeyName foreignKey
        CheckConstraint name _ _ -> name
        IndexConstraint name _ _ -> name
        UsingIndexConstraint name _ _ _ -> name
        OtherConstraint name -> name

constraintColumnsOf :: Constraint -> [Ident]
constraintColumnsOf constraint =
    case constraint of
        PrimaryKeyConstraint _ columns -> columns
        UniqueConstraint _ columns -> columns
        ForeignKeyConstraint foreignKey -> foreignKeyChildColumns foreignKey
        CheckConstraint _ columns _ -> columns
        IndexConstraint _ columns _ -> columns
        UsingIndexConstraint {} -> []
        OtherConstraint _ -> []

{- | Rename the columns owned by the table that defines the constraint. For a
foreign key that is only the child side: the referenced parent columns belong
to another table and must not change. Self-referential foreign keys are
updated on both sides by 'processRename'.
-}
renameConstraintChildColumns :: (Ident -> Ident) -> Constraint -> Constraint
renameConstraintChildColumns rename constraint =
    case constraint of
        PrimaryKeyConstraint name columns ->
            PrimaryKeyConstraint name (map rename columns)
        UniqueConstraint name columns ->
            UniqueConstraint name (map rename columns)
        ForeignKeyConstraint foreignKey ->
            ForeignKeyConstraint
                foreignKey
                    { foreignKeyChildColumns = map rename (foreignKeyChildColumns foreignKey)
                    }
        CheckConstraint name columns notValid ->
            CheckConstraint name (map rename columns) notValid
        IndexConstraint name columns unique ->
            IndexConstraint name (map rename columns) unique
        UsingIndexConstraint name index primary placement ->
            UsingIndexConstraint name index primary placement
        OtherConstraint name -> OtherConstraint name

-- ----------------------------------------------------------------------------
-- Catalog operation dispatch
-- ----------------------------------------------------------------------------

processOp :: StatementId -> Eval -> CatalogOp -> Eval
processOp sid eval op =
    case op of
        DefineTableOp definition -> processDefineTable sid eval definition
        DefineViewOp definition -> processDefineView sid eval definition
        DefineIndexOp definition -> processDefineIndex sid eval definition
        DefineTriggerOp definition -> processDefineTrigger sid eval definition
        DefineSequenceOp definition -> processDefineSequence sid eval definition
        DefineTypeOp definition -> processDefineType sid eval definition
        DefineSchemaOp definition -> processDefineSchema sid eval definition
        ReadOp readFact -> processRead sid eval readFact
        WriteDataOp write -> processDataWrite sid eval write
        WriteSequenceOp write -> processSequenceWrite sid eval write
        AlterTableOp alter -> processAlterTable sid eval alter
        DropCatalogOp dropOp -> processDrop sid eval dropOp
        RenameOp renameOp -> processRename sid eval renameOp
        RefreshCatalogOp refresh -> processRefresh sid eval refresh

-- ----------------------------------------------------------------------------
-- Definitions
-- ----------------------------------------------------------------------------

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

installConstraints ::
    SessionState ->
    StatementId ->
    RelationKey ->
    [Constraint] ->
    Catalog ->
    (Catalog, Set ConstraintKey)
installConstraints session sid key constraints catalog =
    let (catalog', keys, _) =
            foldl' install (catalog, Set.empty, catalogNextUnnamedConstraint catalog) constraints
     in (catalog', keys)
    where
        install (current, keys, next) constraint =
            let resolved = resolveUsingIndex session current key constraint
                (identity, next') =
                    case constraintNameIdent resolved of
                        Just name -> (NamedConstraint name, next)
                        Nothing -> (UnnamedConstraint sid next, next + 1)
                constraintKey = ConstraintKey key identity
                state = ConstraintState key (Just sid) resolved Nothing
             in ( current
                    { catalogConstraints =
                        Map.insert constraintKey state (catalogConstraints current)
                    , catalogNextUnnamedConstraint = next'
                    }
                , Set.insert constraintKey keys
                , next'
                )

{- | Resolve @PRIMARY KEY USING INDEX@ / @UNIQUE USING INDEX@ to a concrete
key constraint using the index's columns. An index that cannot be found is
kept as an opaque constraint: the server would reject the statement, and the
graph builder must not invent columns for it.
-}
resolveUsingIndex :: SessionState -> Catalog -> RelationKey -> Constraint -> Constraint
resolveUsingIndex session catalog tableKey constraint =
    case constraint of
        UsingIndexConstraint name index role placement ->
            let unqualifiedIndexKeys =
                    resolveIndexKeys session catalog (NamedIndex (Name [index]))
                tableName = relationKeyName tableKey
                -- The frontend records whether an unqualified index name
                -- resolves in the altered table's schema or in the current
                -- schema.
                tableSchemaIndexKeys =
                    case nameParts tableName of
                        _schema : _table : _ ->
                            resolveIndexKeys
                                session
                                catalog
                                (NamedIndex (Name (init (nameParts tableName) ++ [index])))
                        _ -> []
                indexKeys =
                    case placement of
                        IndexTableSchema ->
                            case tableSchemaIndexKeys of
                                [] -> unqualifiedIndexKeys
                                keys -> keys
                        IndexCurrentSchema -> unqualifiedIndexKeys
                columns =
                    concat
                        [ indexStateColumns state
                        | indexKey <- indexKeys
                        , Just state <- [Map.lookup indexKey (catalogIndexes catalog)]
                        ]
             in if null columns
                    then OtherConstraint name
                    else
                        if role == PrimaryIndex
                            then PrimaryKeyConstraint name columns
                            else UniqueConstraint name columns
        other -> other

tableConstraintValues :: TableDef -> [Constraint]
tableConstraintValues definition =
    tableConstraints definition
        ++ [ ForeignKeyConstraint foreignKey
           | column <- tableColumns definition
           , foreignKey <- columnReferences column
           ]

foreignKeyParentDeps :: SessionState -> Catalog -> [Constraint] -> Set StatementId
foreignKeyParentDeps session catalog constraints =
    Set.unions
        [ relationReadDeps session catalog parentKey ReadSchema []
        | ForeignKeyConstraint foreignKey <- constraints
        , parentKey <- resolveRelationKeys session catalog (foreignKeyParent foreignKey)
        ]

resolvedSourceKeys :: SessionState -> Catalog -> [ReadFact] -> Set RelationKey
resolvedSourceKeys session catalog readFacts =
    Set.fromList
        [ key
        | readFact <- readFacts
        , key <- resolveRelationKeys session catalog (readSource readFact)
        , Map.member key (catalogRelations catalog)
        ]

unresolvedSourceRefs :: SessionState -> Catalog -> [ReadFact] -> Set NameRef
unresolvedSourceRefs session catalog readFacts =
    Set.fromList
        [ readSource readFact
        | readFact <- readFacts
        , null
            [ key
            | key <- resolveRelationKeys session catalog (readSource readFact)
            , Map.member key (catalogRelations catalog)
            ]
        ]

processDefineTable :: StatementId -> Eval -> TableDef -> Eval
processDefineTable sid eval definition =
    let session = evalSession eval
        catalog = evalCatalog eval
        key =
            RelationKey
                (tableTemporary definition)
                (qualifyDefinitionName session (tableName definition) (tableTemporary definition))
        existing = lookupRelation catalog key
        evalReads = foldl' (processRead sid) eval (tableSources definition)
        likeDeps =
            case tableLike definition of
                Nothing -> Set.empty
                Just reference ->
                    Set.unions
                        [ relationReadDeps session catalog source ReadSchema []
                        | source <- resolveRelationKeys session catalog reference
                        ]
        existingDeps =
            if isJust existing
                then relationAllDeps session catalog key
                else Set.empty
        constraintValues = tableConstraintValues definition
        parentDeps = foreignKeyParentDeps session catalog constraintValues
        eval1 =
            addTouches
                (relationTouchKeys key)
                ( addDeps
                    ( likeDeps
                        `Set.union` existingDeps
                        `Set.union` parentDeps
                        `Set.union` schemaPrefixDeps catalog (relationKeyName key)
                    )
                    evalReads
                )
        unresolved = unresolvedSourceRefs session catalog (tableSources definition)
        eval2 =
            foldl'
                (flip addBarrier)
                eval1
                [ ScopedBarrier
                    (RelationScope reference)
                    UnresolvedViewSourceBarrier
                    (Just ("unresolved source of table " <> nameText (tableName definition)))
                | reference <- Set.toList unresolved
                ]
     in if tableIfNotExists definition && isJust existing
            then eval2
            else
                let catalog1 = insertRelation key (newRelation TableRelation) (clearRelationObjects key catalog)
                    (catalog2, constraintKeys) =
                        installConstraints session sid key constraintValues catalog1
                    relation =
                        (newRelation TableRelation)
                            { relDefinition = Just sid
                            , relColumns =
                                Set.fromList (map columnName (tableColumns definition))
                            , relSources =
                                resolvedSourceKeys session catalog (tableSources definition)
                            , relUnresolvedSources = unresolved
                            , relSourceReads = tableSources definition
                            , relOnCommit = tableOnCommit definition
                            , relOnCommitFrameId =
                                case tableOnCommit definition of
                                    Just OnCommitDrop -> evalFrameId eval
                                    _ -> Nothing
                            , relTransactional = tableTransactional definition
                            , relConstraints = constraintKeys
                            }
                    catalog3 = insertRelation key relation catalog2
                 in eval2 {evalCatalog = catalog3}

processDefineView :: StatementId -> Eval -> ViewDef -> Eval
processDefineView sid eval definition =
    let session = evalSession eval
        catalog = evalCatalog eval
        key =
            RelationKey
                (viewTemporary definition)
                (qualifyDefinitionName session (viewName definition) (viewTemporary definition))
        existing = lookupRelation catalog key
        evalReads = foldl' (processRead sid) eval (viewSources definition)
        existingDeps =
            if isJust existing
                then relationAllDeps session catalog key
                else Set.empty
        eval1 =
            addTouches
                (relationTouchKeys key)
                (addDeps (existingDeps `Set.union` schemaPrefixDeps catalog (relationKeyName key)) evalReads)
        unresolved = unresolvedSourceRefs session catalog (viewSources definition)
        eval2 =
            foldl'
                (flip addBarrier)
                eval1
                [ ScopedBarrier
                    (RelationScope reference)
                    UnresolvedViewSourceBarrier
                    (Just ("unresolved source of view " <> nameText (viewName definition)))
                | reference <- Set.toList unresolved
                ]
     in if viewIfNotExists definition && isJust existing
            then eval2
            else
                let kind =
                        if viewMaterialized definition
                            then MaterializedViewRelation
                            else ViewRelation
                    relation =
                        (newRelation kind)
                            { relDefinition = Just sid
                            , relColumns = Set.fromList (viewColumns definition)
                            , relSources =
                                resolvedSourceKeys session catalog (viewSources definition)
                            , relUnresolvedSources = unresolved
                            , relSourceReads = viewSources definition
                            }
                    catalog1 = insertRelation key relation (clearRelationObjects key catalog)
                 in eval2 {evalCatalog = catalog1}

processDefineIndex :: StatementId -> Eval -> IndexDef -> Eval
processDefineIndex sid eval definition =
    let session = evalSession eval
        catalog = evalCatalog eval
        tableKeys = resolveRelationKeys session catalog (indexTable definition)
        -- The frontend records whether an unqualified index name resolves in
        -- the table's schema or in the current schema.
        indexKeys =
            case (indexRef definition, indexSchemaPlacement definition, tableKeys) of
                (NamedIndex name@(Name [_]), IndexTableSchema, tableKey : _)
                    | length (nameParts (relationKeyName tableKey)) > 1 ->
                        [ NamedIndexKey
                            ( Name
                                ( init (nameParts (relationKeyName tableKey))
                                    ++ nameParts name
                                )
                            )
                        ]
                _ -> resolveIndexKeys session catalog (indexRef definition)
        tableDeps =
            Set.unions
                [ relationReadDeps session catalog tableKey ReadData (indexColumns definition)
                | tableKey <- tableKeys
                ]
        stepKey current indexKey =
            let existing = Map.lookup indexKey (catalogIndexes (evalCatalog current))
                existingDeps =
                    maybe
                        Set.empty
                        (Set.fromList . catMaybes . (\state -> [indexDefinition state, indexRestoredBy state]))
                        existing
                ownerName =
                    case indexKey of
                        NamedIndexKey name -> name
                        TableIndexKey table _ -> relationKeyName table
                current1 =
                    addTouches
                        (indexTouchKeys indexKey (listToMaybe tableKeys))
                        (addDeps (tableDeps `Set.union` existingDeps `Set.union` schemaPrefixDeps catalog ownerName) current)
             in if indexIfNotExists definition && isJust existing
                    then current1
                    else
                        let oldTableKeys =
                                case existing >>= indexStateTable of
                                    Nothing -> []
                                    Just oldTableKey -> [oldTableKey]
                            catalog0 =
                                foldl'
                                    ( \catalog' oldTableKey ->
                                        catalog'
                                            { catalogRelations =
                                                Map.adjust
                                                    (\relation -> relation {relIndexes = Set.delete indexKey (relIndexes relation)})
                                                    oldTableKey
                                                    (catalogRelations catalog')
                                            }
                                    )
                                    (evalCatalog current1)
                                    oldTableKeys
                            catalog1 =
                                catalog0
                                    { catalogIndexes =
                                        Map.insert
                                            indexKey
                                            (IndexState (Just sid) (listToMaybe tableKeys) (indexColumns definition) Nothing)
                                            (catalogIndexes catalog0)
                                    }
                            catalog2 =
                                foldl'
                                    ( \catalog' tableKey ->
                                        catalog'
                                            { catalogRelations =
                                                Map.adjust
                                                    (\relation -> relation {relIndexes = Set.insert indexKey (relIndexes relation)})
                                                    tableKey
                                                    (catalogRelations catalog')
                                            }
                                    )
                                    catalog1
                                    tableKeys
                         in current1 {evalCatalog = catalog2}
     in case tableKeys of
            _ : _ : _ -> ambiguousMutation eval
            _ -> mutateResolved stepKey eval indexKeys

triggerBodyDeps :: SessionState -> Catalog -> TriggerBody -> Set StatementId
triggerBodyDeps session catalog body =
    case body of
        TriggerInline effects ->
            Set.unions (map effectDeps effects)
        TriggerFunction _ _ -> Set.empty
        TriggerOpaque -> Set.empty
    where
        effectDeps effect =
            case effect of
                TriggerDml event ->
                    Set.unions
                        [ relationAllDeps session catalog key
                        | key <- resolveRelationKeys session catalog (dmlEventTarget event)
                        ]
                TriggerRead readFact ->
                    Set.unions
                        [ relationReadDeps session catalog key (readMode readFact) (readColumns readFact)
                        | key <- resolveRelationKeys session catalog (readSource readFact)
                        ]
                TriggerWriteSequence write ->
                    sequenceWriteDeps session catalog (sequenceWriteTarget write)
                TriggerSession _ -> Set.empty
                TriggerBarrier _ -> Set.empty

dmlEventTarget :: DmlEvent -> NameRef
dmlEventTarget event =
    case event of
        DmlInsert target _ -> target
        DmlUpdate target _ -> target
        DmlMerge target _ -> target
        DmlDelete target _ -> target
        DmlTruncate target _ -> target

sequenceWriteDeps :: SessionState -> Catalog -> NameRef -> Set StatementId
sequenceWriteDeps session catalog target =
    Set.unions
        [ Set.fromList
            ( catMaybes
                [ sequenceDefinition state
                , sequenceRestoredBy state
                ]
            )
            `Set.union` sequenceWrites state
        | key <- resolveSequenceKeys session catalog target
        , Just state <- [Map.lookup key (catalogSequences catalog)]
        ]

processDefineTrigger :: StatementId -> Eval -> TriggerDef -> Eval
processDefineTrigger sid eval definition =
    let session = evalSession eval
        catalog = evalCatalog eval
        triggerKeys = resolveTriggerKeys session catalog (triggerRef definition)
        tableKeys = resolveRelationKeys session catalog (triggerTable definition)
        tableDeps =
            Set.unions
                [ relationAllDeps session catalog tableKey
                | tableKey <- tableKeys
                ]
        referencedDeps =
            case triggerReferencedTable definition of
                Nothing -> Set.empty
                Just reference ->
                    Set.unions
                        [ relationAllDeps session catalog tableKey
                        | tableKey <- resolveRelationKeys session catalog reference
                        ]
        bodyDeps = triggerBodyDeps session catalog (triggerBody definition)
        whenDeps =
            Set.unions
                [ relationReadDeps session catalog key (readMode readFact) (readColumns readFact)
                | readFact <- triggerWhenReads definition
                , key <- resolveRelationKeys session catalog (readSource readFact)
                ]
        eval0 =
            case triggerBody definition of
                TriggerFunction name _ ->
                    -- The function body is opaque to the catalog model. Treat
                    -- the trigger definition like an unknown statement so the
                    -- function definition and other prior catalog state are
                    -- retained.
                    addBarrier
                        ( ScopedBarrier
                            GlobalScope
                            UnsupportedTriggerBodyBarrier
                            (Just ("trigger function " <> nameText name))
                        )
                        eval
                _ -> eval
        eval1 =
            addTouches
                (concatMap (`triggerTouchKeys` listToMaybe tableKeys) triggerKeys)
                (addDeps (tableDeps `Set.union` referencedDeps `Set.union` bodyDeps `Set.union` whenDeps) eval0)
        stepKey current triggerKey =
            let existing = Map.lookup triggerKey (catalogTriggers (evalCatalog current))
                existingDeps =
                    maybe
                        Set.empty
                        (Set.fromList . catMaybes . (\state -> [triggerDefinition state, triggerRestoredBy state]))
                        existing
                ownerName =
                    case triggerKey of
                        NamedTriggerKey name -> name
                        TableTriggerKey table _ -> relationKeyName table
                current1 = addDeps (existingDeps `Set.union` schemaPrefixDeps catalog ownerName) current
             in if triggerIfNotExists definition && isJust existing
                    then current1
                    else
                        let oldTableKeys =
                                case existing >>= triggerStateTable of
                                    Nothing -> []
                                    Just oldTableKey -> [oldTableKey]
                            catalog0 =
                                foldl'
                                    ( \catalog' oldTableKey ->
                                        catalog'
                                            { catalogRelations =
                                                Map.adjust
                                                    (\relation -> relation {relTriggers = Set.delete triggerKey (relTriggers relation)})
                                                    oldTableKey
                                                    (catalogRelations catalog')
                                            }
                                    )
                                    (evalCatalog current1)
                                    oldTableKeys
                            catalog1 =
                                catalog0
                                    { catalogTriggers =
                                        Map.insert
                                            triggerKey
                                            ( TriggerState
                                                (Just sid)
                                                (listToMaybe tableKeys)
                                                (triggerTiming definition)
                                                (triggerEvents definition)
                                                (triggerWhenReads definition)
                                                (triggerBody definition)
                                                Nothing
                                            )
                                            (catalogTriggers catalog0)
                                    }
                            catalog2 =
                                foldl'
                                    ( \catalog' tableKey ->
                                        catalog'
                                            { catalogRelations =
                                                Map.adjust
                                                    (\relation -> relation {relTriggers = Set.insert triggerKey (relTriggers relation)})
                                                    tableKey
                                                    (catalogRelations catalog')
                                            }
                                    )
                                    catalog1
                                    tableKeys
                         in current1 {evalCatalog = catalog2}
     in case tableKeys of
            _ : _ : _ -> ambiguousMutation eval
            _ -> mutateResolved stepKey eval1 triggerKeys

processDefineSequence :: StatementId -> Eval -> SequenceDef -> Eval
processDefineSequence sid eval definition =
    let session = evalSession eval
        catalog = evalCatalog eval
        key =
            SequenceKey
                (sequenceTemporary definition)
                (qualifyDefinitionName session (sequenceName definition) (sequenceTemporary definition))
        existing = Map.lookup key (catalogSequences catalog)
        existingDeps =
            maybe
                Set.empty
                ( Set.fromList
                    . catMaybes
                    . (\sequenceState -> [sequenceDefinition sequenceState, sequenceRestoredBy sequenceState])
                )
                existing
        state =
            SequenceState
                (Just sid)
                Set.empty
                Nothing
                (sequenceTransactional definition)
        catalog1 =
            catalog
                { catalogSequences = Map.insert key state (catalogSequences catalog)
                }
     in ( addTouches
            (sequenceTouchKeys key)
            (addDeps (existingDeps `Set.union` schemaPrefixDeps catalog (sequenceKeyName key)) eval)
        )
            { evalCatalog = catalog1
            }

processDefineType :: StatementId -> Eval -> TypeDef -> Eval
processDefineType sid eval definition =
    let catalog = evalCatalog eval
        name = qualifyObjectName (evalSession eval) (typeName definition)
        existing = Map.lookup name (catalogTypes catalog)
        existingDeps =
            maybe
                Set.empty
                (Set.fromList . catMaybes . (\state -> [definitionStatement state, definitionRestoredBy state]))
                existing
        catalog1 =
            catalog
                { catalogTypes =
                    Map.insert name (DefinitionState (Just sid) Nothing) (catalogTypes catalog)
                }
     in ( addTouches
            (typeTouchKeys name)
            (addDeps (existingDeps `Set.union` schemaPrefixDeps catalog name) eval)
        )
            { evalCatalog = catalog1
            }

processDefineSchema :: StatementId -> Eval -> SchemaDef -> Eval
processDefineSchema sid eval definition =
    let catalog = evalCatalog eval
        name = schemaName definition
        existing = Map.lookup name (catalogSchemas catalog)
        existingDeps =
            maybe
                Set.empty
                (Set.fromList . catMaybes . (\state -> [definitionStatement state, definitionRestoredBy state]))
                existing
        catalog1 =
            if isJust existing
                then catalog
                else
                    catalog
                        { catalogSchemas =
                            Map.insert name (DefinitionState (Just sid) Nothing) (catalogSchemas catalog)
                        }
     in ( addTouches
            (schemaTouchKeys name)
            (addDeps (existingDeps `Set.union` schemaPrefixDeps catalog name) eval)
        )
            { evalCatalog = catalog1
            }

-- ----------------------------------------------------------------------------
-- Reads
-- ----------------------------------------------------------------------------

processRead :: StatementId -> Eval -> ReadFact -> Eval
processRead _ eval readFact =
    let session = evalSession eval
        catalog = evalCatalog eval
        relationKeys =
            filter
                (`Map.member` catalogRelations catalog)
                (resolveRelationKeys session catalog (readSource readFact))
        sequenceKeys =
            filter
                (`Map.member` catalogSequences catalog)
                (resolveSequenceKeys session catalog (readSource readFact))
        evalRelations =
            foldl'
                ( \current key ->
                    addTouches
                        (relationTouchKeys key)
                        ( addDeps
                            (relationReadDeps session catalog key (readMode readFact) (readColumns readFact))
                            current
                        )
                )
                eval
                relationKeys
        evalSequences =
            foldl'
                ( \current key ->
                    addTouches
                        (sequenceTouchKeys key)
                        (addDeps (sequenceReadDeps catalog key) current)
                )
                evalRelations
                sequenceKeys
        unresolvedBarriers =
            [ ScopedBarrier
                (RelationScope reference)
                UnresolvedViewSourceBarrier
                (Just ("unresolved source of " <> nameText (nameRefName (readSource readFact))))
            | key <- relationKeys
            , Just relation <- [Map.lookup key (catalogRelations catalog)]
            , relKind relation == ViewRelation
            , reference <- Set.toList (relUnresolvedSources relation)
            , null
                [ sourceKey
                | sourceKey <- resolveRelationKeys session catalog reference
                , Map.member sourceKey (catalogRelations catalog)
                ]
            ]
        evalBarriers = foldl' (flip addBarrier) evalSequences unresolvedBarriers
     in if null relationKeys && null sequenceKeys
            then
                addTouch
                    (RelationBarrierKey (relationFallbackKey session (readSource readFact)))
                    evalBarriers
            else evalBarriers

sequenceReadDeps :: Catalog -> SequenceKey -> Set StatementId
sequenceReadDeps catalog key =
    case Map.lookup key (catalogSequences catalog) of
        Nothing -> Set.empty
        Just state ->
            Set.fromList
                ( catMaybes
                    [ sequenceDefinition state
                    , sequenceRestoredBy state
                    ]
                )
                `Set.union` sequenceWrites state

-- ----------------------------------------------------------------------------
-- Data writes
-- ----------------------------------------------------------------------------

data DmlTag
    = TagInsert
    | TagUpdate ![Ident]
    | TagDelete
    | TagMerge
    | TagTruncate
    deriving (Eq, Show)

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

dmlTagFor :: WriteKind -> [Ident] -> DmlTag
dmlTagFor kind columns =
    case kind of
        InsertWrite _ -> TagInsert
        UpdateWrite -> TagUpdate columns
        DeleteWrite _ -> TagDelete
        MergeWrite -> TagMerge
        TruncateWrite {} -> TagTruncate

eventMatches :: DmlTag -> TriggerEvent -> Bool
eventMatches tag event =
    case (tag, event) of
        (TagInsert, TriggerInsert) -> True
        (TagUpdate columns, TriggerUpdate triggerColumns) ->
            null columns
                || null triggerColumns
                || not (Set.null (Set.intersection (Set.fromList columns) (Set.fromList triggerColumns)))
        (TagDelete, TriggerDelete) -> True
        -- A MERGE may execute INSERT, UPDATE, and DELETE actions, so it may
        -- fire any of those trigger events.
        (TagMerge, TriggerInsert) -> True
        (TagMerge, TriggerUpdate _) -> True
        (TagMerge, TriggerDelete) -> True
        (TagTruncate, TriggerTruncate) -> True
        _ -> False

matchingTriggers :: Catalog -> RelationKey -> DmlTag -> [(TriggerKey, TriggerState)]
matchingTriggers catalog key tag =
    case lookupRelation catalog key of
        Nothing -> []
        Just relation ->
            [ (triggerKey, trigger)
            | triggerKey <- Set.toList (relTriggers relation)
            , Just trigger <- [Map.lookup triggerKey (catalogTriggers catalog)]
            , any (eventMatches tag) (triggerStateEvents trigger)
            , triggerStateTiming trigger /= TriggerInsteadOf
                || relKind relation `elem` [ViewRelation, MaterializedViewRelation]
            ]

processDataWrite :: StatementId -> Eval -> DataWrite -> Eval
processDataWrite sid eval write =
    let evalReads = foldl' (processRead sid) eval (dataWriteReads write)
        session = evalSession evalReads
        catalog = evalCatalog evalReads
        targetKeys = resolveRelationKeys session catalog (dataWriteTarget write)
        unqualified = dataWriteUnqualifiedColumns write
        writeColumns key =
            let knownColumns =
                    maybe Set.empty relColumns (lookupRelation catalog key)
                resolved
                    | Set.null knownColumns = unqualified
                    | otherwise = filter (`Set.member` knownColumns) unqualified
             in dataWriteColumns write ++ resolved
        applyTarget current key =
            let columns = writeColumns key
             in -- An unqualified assignment that names none of this target's
                -- columns belongs to a different table in a multi-table update.
                if null columns && not (null unqualified)
                    then current
                    else writeRelation sid current key (dataWriteKind write) columns
     in mutateResolved applyTarget evalReads targetKeys

writeRelation :: StatementId -> Eval -> RelationKey -> WriteKind -> [Ident] -> Eval
writeRelation sid eval key kind columns =
    let session = evalSession eval
        catalog = evalCatalog eval
        relation = fromMaybe (newRelation TableRelation) (lookupRelation catalog key)
        tag = dmlTagFor kind columns
        matching = matchingTriggers catalog key tag
        insteadOf = [triggerKey | (triggerKey, trigger) <- matching, triggerStateTiming trigger == TriggerInsteadOf]
        isView = relKind relation `elem` [ViewRelation, MaterializedViewRelation]
        suppressOriginal = isView && not (null insteadOf)
        signature = writeSignature kind columns
        evalMarked =
            eval
                { evalVisitedWrites =
                    Set.insert (key, signature) (evalVisitedWrites eval)
                }
        evalDeps =
            addTouches
                (relationTouchKeys key)
                (addDeps (relationWriteDeps session catalog key kind columns) evalMarked)
        evalWrite =
            if suppressOriginal
                then evalDeps
                else
                    evalDeps
                        { evalCatalog =
                            insertRelation key (applyWriteState sid kind columns relation) (evalCatalog evalDeps)
                        }
        evalTriggers = foldl' (\current (triggerKey, _) -> expandTrigger sid current triggerKey) evalWrite matching
        evalCascades = applyForeignKeys sid evalTriggers key kind columns
     in if isView && null insteadOf
            then addViewWriteBarriers evalCascades key
            else evalCascades

addViewWriteBarriers :: Eval -> RelationKey -> Eval
addViewWriteBarriers eval key =
    foldl'
        (flip addBarrier)
        eval
        [ ScopedBarrier
            (RelationScope (NameRef (relationKeyName source) (relationKeyTemp source)))
            WriteThroughViewBarrier
            (Just "write through view")
        | source <- Set.toList (transitiveSources (evalCatalog eval) key)
        ]

transitiveSources :: Catalog -> RelationKey -> Set RelationKey
transitiveSources catalog = go Set.empty
    where
        go seen current
            | Set.member current seen = Set.empty
            | otherwise =
                case lookupRelation catalog current of
                    Nothing -> Set.singleton current
                    Just relation ->
                        Set.insert current $
                            Set.unions
                                [ go (Set.insert current seen) source
                                | source <- Set.toList (relSources relation)
                                ]

-- ----------------------------------------------------------------------------
-- Triggers
-- ----------------------------------------------------------------------------

expandTrigger :: StatementId -> Eval -> TriggerKey -> Eval
expandTrigger sid eval triggerKey
    | Set.member triggerKey (evalVisitedTriggers eval) = eval
    | otherwise =
        case Map.lookup triggerKey (catalogTriggers (evalCatalog eval)) of
            Nothing -> eval
            Just trigger ->
                let tableKey = triggerStateTable trigger
                    eval1 =
                        eval
                            { evalVisitedTriggers =
                                Set.insert triggerKey (evalVisitedTriggers eval)
                            }
                    definitionDeps =
                        Set.fromList
                            ( catMaybes
                                [ triggerDefinition trigger
                                , triggerRestoredBy trigger
                                ]
                            )
                    eval2 =
                        addTouches
                            (triggerTouchKeys triggerKey tableKey)
                            (addDeps definitionDeps eval1)
                    eval3 =
                        foldl'
                            (processRead sid)
                            eval2
                            (triggerStateWhenReads trigger)
                 in case triggerStateBody trigger of
                        TriggerInline effects ->
                            foldl' (processTriggerEffect sid) eval3 effects
                        TriggerFunction name _ ->
                            addBarrier
                                ( ScopedBarrier
                                    GlobalScope
                                    UnsupportedTriggerBodyBarrier
                                    (Just ("trigger function " <> nameText name))
                                )
                                eval3
                        TriggerOpaque ->
                            addBarrier
                                (ScopedBarrier GlobalScope OpaqueTriggerBodyBarrier Nothing)
                                eval3

processTriggerEffect :: StatementId -> Eval -> TriggerEffect -> Eval
processTriggerEffect sid eval effect =
    case effect of
        TriggerDml event -> processDmlEvent sid eval event
        TriggerRead readFact -> processRead sid eval readFact
        TriggerWriteSequence write -> processSequenceWrite sid eval write
        TriggerSession sessionEffect ->
            let (eval', _) = applySessionEffects sid [sessionEffect] eval
             in eval' {evalSessionTouched = True}
        TriggerBarrier scoped -> addBarrier scoped eval

processDmlEvent :: StatementId -> Eval -> DmlEvent -> Eval
processDmlEvent sid eval event =
    let session = evalSession eval
        catalog = evalCatalog eval
        (target, kind, columns) =
            case event of
                DmlInsert target' columns' ->
                    (target', InsertWrite ConflictError, columns')
                DmlUpdate target' columns' ->
                    (target', UpdateWrite, columns')
                DmlMerge target' columns' ->
                    (target', MergeWrite, columns')
                DmlDelete target' scope ->
                    (target', DeleteWrite scope, [])
                DmlTruncate target' behavior ->
                    (target', TruncateWrite behavior ContinueIdentity, [])
     in mutateResolved
            (\current key -> writeRelation sid current key kind columns)
            eval
            (resolveRelationKeys session catalog target)

-- ----------------------------------------------------------------------------
-- Sequences
-- ----------------------------------------------------------------------------

processSequenceWrite :: StatementId -> Eval -> SequenceWrite -> Eval
processSequenceWrite sid eval write =
    let session = evalSession eval
        catalog = evalCatalog eval
        keys = resolveSequenceKeys session catalog (sequenceWriteTarget write)
        transactional = sequenceWriteTransactional write
        stepKey current key =
            let existing = Map.lookup key (catalogSequences (evalCatalog current))
                state =
                    fromMaybe
                        (SequenceState Nothing Set.empty Nothing transactional)
                        existing
                deps =
                    Set.fromList
                        ( catMaybes
                            [ sequenceDefinition state
                            , sequenceRestoredBy state
                            ]
                        )
                        `Set.union` sequenceWrites state
                state' = state {sequenceWrites = Set.insert sid (sequenceWrites state)}
                catalog1 =
                    (evalCatalog current)
                        { catalogSequences =
                            Map.insert key state' (catalogSequences (evalCatalog current))
                        }
             in (addTouches (sequenceTouchKeys key) (addDeps deps current))
                    { evalCatalog = catalog1
                    }
        evalWrites = mutateResolved stepKey eval keys
     in if not transactional && evalTransactionActive eval
            then
                -- NOTE: durable sequence values across DDL rollback need
                -- generation history; retain the script instead of restoring
                -- their values as if they were ordinary transactional rows.
                addBarrier
                    ( ScopedBarrier
                        GlobalScope
                        UnsupportedTransactionBarrier
                        (Just "nontransactional sequence values in a transaction")
                    )
                    evalWrites
            else evalWrites

-- ----------------------------------------------------------------------------
-- Foreign keys
-- ----------------------------------------------------------------------------

applyForeignKeys :: StatementId -> Eval -> RelationKey -> WriteKind -> [Ident] -> Eval
applyForeignKeys sid eval key kind columns
    | not (sessForeignKeyChecks (evalSession eval)) = eval
    | otherwise =
        let evalChecks = applyChildChecks sid eval key kind columns
            evalActions = applyParentActions sid evalChecks key kind columns
         in evalActions

applyChildChecks :: StatementId -> Eval -> RelationKey -> WriteKind -> [Ident] -> Eval
applyChildChecks _sid eval key kind _columns =
    case kind of
        InsertWrite _ -> checkParents
        UpdateWrite -> checkParents
        MergeWrite -> checkParents
        _ -> eval
    where
        catalog = evalCatalog eval
        session = evalSession eval
        checkParents =
            foldl'
                ( \current (_, state, foreignKey) ->
                    if constraintStateTable state == key
                        then
                            addDeps
                                ( Set.unions
                                    [ relationReadDeps session catalog parentKey ReadData []
                                    | parentKey <-
                                        resolveRelationKeys
                                            session
                                            catalog
                                            (foreignKeyParent foreignKey)
                                    ]
                                )
                                current
                        else current
                )
                eval
                (allForeignKeys catalog)

applyParentActions :: StatementId -> Eval -> RelationKey -> WriteKind -> [Ident] -> Eval
applyParentActions sid eval key kind columns =
    case kind of
        DeleteWrite _ ->
            foldl'
                (\current item -> parentAction current key (DeleteWrite DeleteAllRows) [] item)
                eval
                (allForeignKeys catalog)
        UpdateWrite ->
            foldl'
                (\current item -> parentAction current key UpdateWrite columns item)
                eval
                (allForeignKeys catalog)
        TruncateWrite behavior _ ->
            if behavior == TruncateCascade
                then
                    foldl'
                        ( \current (_, state, foreignKey) ->
                            if parentMatches state foreignKey
                                then
                                    writeChild
                                        current
                                        (constraintStateTable state)
                                        (TruncateWrite TruncateCascade ContinueIdentity)
                                        []
                                else current
                        )
                        eval
                        (allForeignKeys catalog)
                else
                    -- Without CASCADE the server refuses to truncate a parent
                    -- that is still referenced. The statement therefore depends
                    -- on the referencing table and its foreign key constraint.
                    foldl'
                        ( \current (_, state, foreignKey) ->
                            if parentMatches state foreignKey
                                then
                                    let childKey = constraintStateTable state
                                        constraintDeps =
                                            Set.fromList
                                                ( catMaybes
                                                    [ constraintDefinition state
                                                    , constraintRestoredBy state
                                                    ]
                                                )
                                        childDeps =
                                            relationReadDeps
                                                session
                                                catalog
                                                childKey
                                                ReadSchema
                                                []
                                     in addDeps (childDeps `Set.union` constraintDeps) current
                                else current
                        )
                        eval
                        (allForeignKeys catalog)
        _ -> eval
    where
        catalog = evalCatalog eval
        session = evalSession eval

        parentMatches _state foreignKey =
            key
                `elem` resolveRelationKeys
                    session
                    catalog
                    (foreignKeyParent foreignKey)

        parentAction current parentKey action updatedParentColumns (_, state, foreignKey)
            | not (parentMatches state foreignKey) = current
            | not (foreignKeyEventMatches action updatedParentColumns parentKey state foreignKey) = current
            | otherwise =
                case referentialAction action foreignKey of
                    Cascade ->
                        case action of
                            DeleteWrite _ ->
                                writeChild current (constraintStateTable state) (DeleteWrite DeleteAllRows) []
                            UpdateWrite ->
                                writeChild
                                    current
                                    (constraintStateTable state)
                                    UpdateWrite
                                    (foreignKeyChildColumns foreignKey)
                            _ -> current
                    SetNull ->
                        writeChild
                            current
                            (constraintStateTable state)
                            UpdateWrite
                            (foreignKeyChildColumns foreignKey)
                    SetDefault ->
                        writeChild
                            current
                            (constraintStateTable state)
                            UpdateWrite
                            (foreignKeyChildColumns foreignKey)
                    Restrict ->
                        addDeps
                            (relationReadDeps session catalog (constraintStateTable state) ReadData [])
                            current
                    NoAction ->
                        addDeps
                            (relationReadDeps session catalog (constraintStateTable state) ReadData [])
                            current

        foreignKeyEventMatches action updatedParentColumns _parentKey _state foreignKey =
            case action of
                DeleteWrite _ -> True
                UpdateWrite ->
                    let relevant = referencedParentColumns session catalog foreignKey
                     in null updatedParentColumns
                            || null relevant
                            || not
                                ( Set.null
                                    ( Set.intersection
                                        (Set.fromList updatedParentColumns)
                                        (Set.fromList relevant)
                                    )
                                )
                _ -> False

        referentialAction action foreignKey =
            case action of
                DeleteWrite _ -> foreignKeyOnDelete foreignKey
                UpdateWrite -> foreignKeyOnUpdate foreignKey
                _ -> NoAction

        writeChild current childKey childKind childColumns =
            let signature = writeSignature childKind childColumns
             in if Set.member (childKey, signature) (evalVisitedWrites current)
                    then current
                    else writeRelation sid current childKey childKind childColumns

-- ----------------------------------------------------------------------------
-- ALTER TABLE
-- ----------------------------------------------------------------------------

relationConstraintDeps :: Catalog -> RelationState -> Set StatementId
relationConstraintDeps catalog relation =
    Set.unions
        [ Set.fromList
            ( catMaybes
                [ constraintDefinition state
                , constraintRestoredBy state
                ]
            )
        | key <- Set.toList (relConstraints relation)
        , Just state <- [Map.lookup key (catalogConstraints catalog)]
        ]

introducedForeignKeysOf :: AlterChange -> [ForeignKey]
introducedForeignKeysOf change =
    case change of
        AddConstraint (ForeignKeyConstraint foreignKey) -> [foreignKey]
        AddColumn column constraints ->
            [foreignKey | ForeignKeyConstraint foreignKey <- constraints]
                ++ columnReferences column
        _ -> []

{- | Views that read the dropped column, directly or through another view.
The retained read columns make this precise for direct reads; transitive
view dependencies are over-approximated because column lineage is not
modelled.
-}
dropColumnDependentViews :: SessionState -> Catalog -> RelationKey -> Ident -> [RelationKey]
dropColumnDependentViews session catalog tableKey column =
    [ viewKey
    | viewKey <- Map.keys (catalogRelations catalog)
    , viewKey /= tableKey
    , viewDependsOnColumn session catalog Set.empty tableKey column viewKey
    ]

viewDependsOnColumn ::
    SessionState ->
    Catalog ->
    Set RelationKey ->
    RelationKey ->
    Ident ->
    RelationKey ->
    Bool
viewDependsOnColumn session catalog visited tableKey column viewKey
    | Set.member viewKey visited = False
    | otherwise =
        case Map.lookup viewKey (catalogRelations catalog) of
            Just relation
                | relKind relation `elem` [ViewRelation, MaterializedViewRelation] ->
                    let seen = Set.insert viewKey visited
                        sourceReads =
                            [ (readFact, resolveRelationKeys session catalog (readSource readFact))
                            | readFact <- relSourceReads relation
                            ]
                        direct =
                            any
                                ( \(readFact, keys) ->
                                    tableKey `elem` keys
                                        && (null (readColumns readFact) || column `elem` readColumns readFact)
                                )
                                sourceReads
                        transitive =
                            any
                                ( \(_, keys) ->
                                    any
                                        ( \sourceKey ->
                                            sourceKey /= tableKey
                                                && viewDependsOnColumn session catalog seen tableKey column sourceKey
                                        )
                                        keys
                                )
                                sourceReads
                        unresolved =
                            any
                                ( \reference ->
                                    tableKey `elem` resolveRelationKeys session catalog reference
                                )
                                (Set.toList (relUnresolvedSources relation))
                        noReadInfo =
                            tableKey `Set.member` relSources relation
                                && not (any (\(_, keys) -> tableKey `elem` keys) sourceReads)
                     in direct || transitive || unresolved || noReadInfo
            _ -> False

processAlterTable :: StatementId -> Eval -> AlterTable -> Eval
processAlterTable sid eval alter =
    let session = evalSession eval
        catalog = evalCatalog eval
        keys = resolveRelationKeys session catalog (alterTableTarget alter)
        changes = alterTableChanges alter
        introducedForeignKeys =
            concatMap introducedForeignKeysOf changes
        foreignKeyDeps =
            Set.unions
                [ relationReadDeps
                    session
                    catalog
                    parentKey
                    ( if foreignKeyNotValid foreignKey
                        then ReadSchema
                        else ReadData
                    )
                    []
                | foreignKey <- introducedForeignKeys
                , parentKey <- resolveRelationKeys session catalog (foreignKeyParent foreignKey)
                ]
        stepKey current key =
            let currentCatalog = evalCatalog current
                relation = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog key)
                validationDeps =
                    Set.unions
                        [ relationReadDeps session currentCatalog parentKey ReadData []
                        | ValidateConstraint name <- changes
                        , constraintKey <- Set.toList (relConstraints relation)
                        , constraintKeyIdentity constraintKey == NamedConstraint name
                        , Just state <- [Map.lookup constraintKey (catalogConstraints currentCatalog)]
                        , ForeignKeyConstraint foreignKey <- [constraintValue state]
                        , parentKey <-
                            resolveRelationKeys
                                session
                                currentCatalog
                                (foreignKeyParent foreignKey)
                        ]
                dropColumnViewDeps =
                    Set.unions
                        [ Set.unions
                            [ relationAllDeps session currentCatalog viewKey
                                `Set.union` relationConstraintDeps currentCatalog viewRelation
                            | viewKey <- dropColumnDependentViews session currentCatalog key column
                            , Just viewRelation <- [lookupRelation currentCatalog viewKey]
                            ]
                        | DropColumn column _ <- changes
                        ]
                deps =
                    relationAllDeps session currentCatalog key
                        `Set.union` relationConstraintDeps currentCatalog relation
                        `Set.union` foreignKeyDeps
                        `Set.union` validationDeps
                        `Set.union` dropColumnViewDeps
                current1 =
                    addTouches
                        (relationTouchKeys key)
                        (addDeps deps current)
                (catalog1, relation1) =
                    foldl'
                        (applyAlterChange session sid key)
                        (currentCatalog, relation)
                        changes
                relation2 =
                    relation1 {relSchemaWrites = Set.insert sid (relSchemaWrites relation1)}
             in current1 {evalCatalog = insertRelation key relation2 catalog1}
     in mutateResolved stepKey eval keys

applyAlterChange ::
    SessionState ->
    StatementId ->
    RelationKey ->
    (Catalog, RelationState) ->
    AlterChange ->
    (Catalog, RelationState)
applyAlterChange session sid key (catalog, relation) change =
    case change of
        AddColumn column constraints ->
            let allConstraints =
                    constraints
                        ++ [ForeignKeyConstraint foreignKey | foreignKey <- columnReferences column]
                (catalog1, keys) = installConstraints session sid key allConstraints catalog
             in ( catalog1
                , relation
                    { relColumns = Set.insert (columnName column) (relColumns relation)
                    , relConstraints = Set.union keys (relConstraints relation)
                    }
                )
        DropColumn column cascade ->
            let isParentColumn foreignKey =
                    column `elem` referencedParentColumns session catalog foreignKey
                        && key
                            `elem` resolveRelationKeys
                                session
                                catalog
                                (foreignKeyParent foreignKey)
                -- Indexes and constraints inside this table are always dropped
                -- with the column, even without CASCADE.
                sameTableKeys =
                    [ constraintKey
                    | constraintKey <- Set.toList (relConstraints relation)
                    , Just state <- [Map.lookup constraintKey (catalogConstraints catalog)]
                    , let constraint = constraintValue state
                    , column `elem` constraintColumnsOf constraint
                        || case constraint of
                            ForeignKeyConstraint foreignKey -> isParentColumn foreignKey
                            _ -> False
                    ]
                sameTableIndexes =
                    [ indexKey
                    | indexKey <- Set.toList (relIndexes relation)
                    , Just state <- [Map.lookup indexKey (catalogIndexes catalog)]
                    , indexStateTable state == Just key
                    , column `elem` indexStateColumns state
                    ]
                -- Foreign keys in other tables that reference this column as
                -- a parent column are external dependents. Without CASCADE the
                -- server rejects the drop, so the whole catalog must stay
                -- unchanged; with CASCADE the constraints are removed.
                crossTableKeys =
                    [ constraintKey
                    | (constraintKey, state) <- Map.toList (catalogConstraints catalog)
                    , constraintStateTable state /= key
                    , ForeignKeyConstraint foreignKey <- [constraintValue state]
                    , isParentColumn foreignKey
                    ]
                -- A self-referential foreign key still makes the dropped
                -- column a referenced parent column. It must block a RESTRICT
                -- drop just like a foreign key from another table.
                selfParentKeys =
                    [ constraintKey
                    | (constraintKey, state) <- Map.toList (catalogConstraints catalog)
                    , constraintStateTable state == key
                    , ForeignKeyConstraint foreignKey <- [constraintValue state]
                    , isParentColumn foreignKey
                    ]
                -- Views retain the columns they read from each source, so a
                -- view that only uses other columns does not block the drop.
                -- A source with no retained read information is treated as
                -- dependent, which is the sound over-approximation.
                dependentViews = dropColumnDependentViews session catalog key column
                hasExternalDependents =
                    not (null crossTableKeys)
                        || not (null selfParentKeys)
                        || not (null dependentViews)
                removedKeys =
                    sameTableKeys
                        ++ if cascade
                            then crossTableKeys ++ selfParentKeys
                            else []
                catalog1 =
                    if cascade
                        then
                            foldl'
                                (flip (removeRelationCascade session sid DropCascade))
                                catalog
                                dependentViews
                        else catalog
             in if not cascade && hasExternalDependents
                    then (catalog, relation)
                    else
                        ( foldl'
                            (flip deleteIndex)
                            (foldl' (flip deleteConstraint) catalog1 removedKeys)
                            sameTableIndexes
                        , relation
                            { relColumns = Set.delete column (relColumns relation)
                            , relColumnWrites = Map.delete column (relColumnWrites relation)
                            , relConstraints =
                                Set.difference (relConstraints relation) (Set.fromList removedKeys)
                            , relIndexes =
                                Set.difference (relIndexes relation) (Set.fromList sameTableIndexes)
                            }
                        )
        AddConstraint constraint ->
            let (catalog1, keys) = installConstraints session sid key [constraint] catalog
             in ( catalog1
                , relation {relConstraints = Set.union keys (relConstraints relation)}
                )
        DropConstraint name cascade ->
            let matching =
                    [ constraintKey
                    | constraintKey <- Set.toList (relConstraints relation)
                    , constraintKeyIdentity constraintKey == NamedConstraint name
                    ]
                -- Only primary-key and unique constraints can be referenced by
                -- a foreign key. The referenced column set decides whether a
                -- given foreign key depends on the dropped constraint; an
                -- omitted parent column list means the parent primary key.
                dropTarget droppedState =
                    case constraintValue droppedState of
                        PrimaryKeyConstraint _ columns ->
                            Just (True, Set.fromList columns)
                        UniqueConstraint _ columns ->
                            Just (False, Set.fromList columns)
                        _ -> Nothing
                dependsOn (primary, targetColumns) keyValue =
                    let parentColumns = Set.fromList (foreignKeyParentColumns keyValue)
                     in key
                            `elem` resolveRelationKeys
                                session
                                catalog
                                (foreignKeyParent keyValue)
                            && if Set.null parentColumns
                                then primary
                                else parentColumns == targetColumns
                dependentKeys =
                    Set.toList . Set.fromList $
                        [ foreignKey
                        | droppedKey <- matching
                        , Just droppedState <- [Map.lookup droppedKey (catalogConstraints catalog)]
                        , Just target <- [dropTarget droppedState]
                        , not (Set.null (snd target))
                        , (foreignKey, state) <- Map.toList (catalogConstraints catalog)
                        , foreignKey `notElem` matching
                        , ForeignKeyConstraint keyValue <- [constraintValue state]
                        , dependsOn target keyValue
                        ]
                removedKeys = matching ++ dependentKeys
                catalog1 = foldl' (flip deleteConstraint) catalog removedKeys
             in if not cascade && not (null dependentKeys)
                    then (catalog, relation)
                    else
                        ( catalog1
                        , relation
                            { relConstraints =
                                Set.difference (relConstraints relation) (Set.fromList removedKeys)
                            }
                        )
        RenameConstraint from to ->
            let matching =
                    [ constraintKey
                    | constraintKey <- Set.toList (relConstraints relation)
                    , constraintKeyIdentity constraintKey == NamedConstraint from
                    ]
                renameOne catalog' constraintKey =
                    case Map.lookup constraintKey (catalogConstraints catalog') of
                        Nothing -> catalog'
                        Just state ->
                            let newKey = ConstraintKey key (NamedConstraint to)
                                newState =
                                    state
                                        { constraintStateTable = key
                                        , constraintValue = renameConstraintName to (constraintValue state)
                                        }
                             in catalog'
                                    { catalogConstraints =
                                        Map.insert
                                            newKey
                                            newState
                                            (Map.delete constraintKey (catalogConstraints catalog'))
                                    }
                catalog1 = foldl' renameOne catalog matching
                newKeys = Set.fromList [ConstraintKey key (NamedConstraint to) | _ <- matching]
             in ( catalog1
                , relation
                    { relConstraints =
                        Set.union
                            newKeys
                            (Set.difference (relConstraints relation) (Set.fromList matching))
                    }
                )
        ValidateConstraint name ->
            let matching =
                    [ constraintKey
                    | constraintKey <- Set.toList (relConstraints relation)
                    , constraintKeyIdentity constraintKey == NamedConstraint name
                    ]
                catalog1 =
                    foldl'
                        ( \current constraintKey ->
                            current
                                { catalogConstraints =
                                    Map.adjust
                                        ( \state ->
                                            state
                                                { constraintValue =
                                                    validateConstraintValue (constraintValue state)
                                                }
                                        )
                                        constraintKey
                                        (catalogConstraints current)
                                }
                        )
                        catalog
                        matching
             in (catalog1, relation)
        DropIndexChange name ->
            let indexKeys =
                    [ indexKey
                    | indexKey <- Set.toList (relIndexes relation)
                    , indexKeyName indexKey == name
                    ]
                catalog1 = foldl' (flip deleteIndex) catalog indexKeys
             in ( catalog1
                , relation
                    { relIndexes =
                        Set.difference (relIndexes relation) (Set.fromList indexKeys)
                    }
                )
        AlterColumn column _detail ->
            (catalog, relation {relColumns = Set.insert column (relColumns relation)})
        OtherChange _ -> (catalog, relation)

renameConstraintName :: Ident -> Constraint -> Constraint
renameConstraintName name constraint =
    case constraint of
        PrimaryKeyConstraint _ columns -> PrimaryKeyConstraint (Just name) columns
        UniqueConstraint _ columns -> UniqueConstraint (Just name) columns
        ForeignKeyConstraint foreignKey ->
            ForeignKeyConstraint foreignKey {foreignKeyName = Just name}
        CheckConstraint _ columns notValid ->
            CheckConstraint (Just name) columns notValid
        IndexConstraint _ columns unique ->
            IndexConstraint (Just name) columns unique
        UsingIndexConstraint _ index primary placement ->
            UsingIndexConstraint (Just name) index primary placement
        OtherConstraint _ -> OtherConstraint (Just name)

{- | Clear @NOT VALID@ after a successful @VALIDATE CONSTRAINT@. Only foreign
key and check constraints carry that flag.
-}
validateConstraintValue :: Constraint -> Constraint
validateConstraintValue constraint =
    case constraint of
        ForeignKeyConstraint foreignKey ->
            ForeignKeyConstraint foreignKey {foreignKeyNotValid = False}
        CheckConstraint name columns _ ->
            CheckConstraint name columns ConstraintValid
        other -> other

indexKeyName :: IndexKey -> Ident
indexKeyName indexKey =
    case indexKey of
        NamedIndexKey (Name parts) ->
            fromMaybe (Ident "") (lastMaybe parts)
        TableIndexKey _ name -> name

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe values = Just (last values)

-- ----------------------------------------------------------------------------
-- DROP
-- ----------------------------------------------------------------------------

processDrop :: StatementId -> Eval -> DropOp -> Eval
processDrop sid eval dropOp =
    foldl' (dropOne sid (dropMode dropOp)) eval (dropObjects dropOp)

{- | Dropping a relation can change how unqualified names resolve, because a
temporary or schema-qualified relation may have been shadowing another one.
Record the drop as a schema write on every relation whose name could have been
affected, so later reads keep a sound dependency on the drop.

The final name component is what an unqualified reference matches; comparing
only full suffixes would miss a schema-qualified relation shadowing a relation
of the same name in another schema.
-}
markResolutionChange :: StatementId -> RelationKey -> Catalog -> Catalog
markResolutionChange sid dropped catalog =
    catalog
        { catalogRelations =
            Map.mapWithKey adjust (catalogRelations catalog)
        }
    where
        droppedName = relationKeyName dropped
        adjust key relation
            | key /= dropped
            , lastPart droppedName == lastPart (relationKeyName key)
                || nameSuffix droppedName (relationKeyName key)
                || nameSuffix (relationKeyName key) droppedName =
                relation {relSchemaWrites = Set.insert sid (relSchemaWrites relation)}
            | otherwise = relation
        lastPart (Name parts) = lastMaybe parts

dropOne :: StatementId -> DropMode -> Eval -> DropObject -> Eval
dropOne sid mode eval object =
    case object of
        DropRelation reference _kind ->
            let session = evalSession eval
                catalog = evalCatalog eval
                keys = resolveRelationKeys session catalog reference
                existing = filter (`Map.member` catalogRelations catalog) keys
             in if null existing
                    then addTouch (RelationBarrierKey (relationFallbackKey session reference)) eval
                    else
                        mutateResolved
                            ( \current key ->
                                let currentCatalog = evalCatalog current
                                    relation = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog key)
                                    deps =
                                        relationAllDeps session currentCatalog key
                                            `Set.union` relationConstraintDeps currentCatalog relation
                                    current1 =
                                        addTouches
                                            (relationTouchKeys key)
                                            (addDeps deps current)
                                 in current1
                                        { evalCatalog =
                                            removeRelationCascade
                                                session
                                                sid
                                                mode
                                                key
                                                (markResolutionChange sid key (evalCatalog current1))
                                        }
                            )
                            eval
                            existing
        DropIndex reference ->
            let session = evalSession eval
                catalog = evalCatalog eval
                keys = resolveIndexKeys session catalog reference
                stepKey current key =
                    case Map.lookup key (catalogIndexes (evalCatalog current)) of
                        Nothing -> current
                        Just state ->
                            let tableDeps =
                                    maybe Set.empty (relationAllDeps session (evalCatalog current)) (indexStateTable state)
                                deps =
                                    Set.fromList
                                        ( catMaybes
                                            [ indexDefinition state
                                            , indexRestoredBy state
                                            ]
                                        )
                                        `Set.union` tableDeps
                                catalog1 =
                                    deleteIndex key (evalCatalog current)
                                catalog2 =
                                    case indexStateTable state of
                                        Nothing -> catalog1
                                        Just tableKey ->
                                            catalog1
                                                { catalogRelations =
                                                    Map.adjust
                                                        (\relation -> relation {relIndexes = Set.delete key (relIndexes relation)})
                                                        tableKey
                                                        (catalogRelations catalog1)
                                                }
                             in (addTouches (indexTouchKeys key (indexStateTable state)) (addDeps deps current))
                                    { evalCatalog = catalog2
                                    }
             in mutateResolved stepKey eval keys
        DropTrigger reference ->
            let session = evalSession eval
                catalog = evalCatalog eval
                keys = resolveTriggerKeys session catalog reference
                stepKey current key =
                    case Map.lookup key (catalogTriggers (evalCatalog current)) of
                        Nothing -> current
                        Just state ->
                            let tableDeps =
                                    maybe Set.empty (relationAllDeps session (evalCatalog current)) (triggerStateTable state)
                                deps =
                                    Set.fromList
                                        ( catMaybes
                                            [ triggerDefinition state
                                            , triggerRestoredBy state
                                            ]
                                        )
                                        `Set.union` tableDeps
                                catalog1 = deleteTrigger key (evalCatalog current)
                                catalog2 =
                                    case triggerStateTable state of
                                        Nothing -> catalog1
                                        Just tableKey ->
                                            catalog1
                                                { catalogRelations =
                                                    Map.adjust
                                                        (\relation -> relation {relTriggers = Set.delete key (relTriggers relation)})
                                                        tableKey
                                                        (catalogRelations catalog1)
                                                }
                             in (addTouches (triggerTouchKeys key (triggerStateTable state)) (addDeps deps current))
                                    { evalCatalog = catalog2
                                    }
             in mutateResolved stepKey eval keys
        DropSequence reference ->
            let session = evalSession eval
                catalog = evalCatalog eval
                keys = resolveSequenceKeys session catalog reference
                stepKey current key =
                    case Map.lookup key (catalogSequences (evalCatalog current)) of
                        Nothing -> current
                        Just state ->
                            let deps =
                                    Set.fromList
                                        ( catMaybes
                                            [ sequenceDefinition state
                                            , sequenceRestoredBy state
                                            ]
                                        )
                                        `Set.union` sequenceWrites state
                             in (addTouches (sequenceTouchKeys key) (addDeps deps current))
                                    { evalCatalog = deleteSequence key (evalCatalog current)
                                    }
             in mutateResolved stepKey eval keys
        DropType name ->
            let catalog = evalCatalog eval
                key = qualifyObjectName (evalSession eval) name
                deps =
                    maybe
                        Set.empty
                        (Set.fromList . catMaybes . (\state -> [definitionStatement state, definitionRestoredBy state]))
                        (Map.lookup key (catalogTypes catalog))
             in (addTouches (typeTouchKeys key) (addDeps deps eval))
                    { evalCatalog =
                        catalog {catalogTypes = Map.delete key (catalogTypes catalog)}
                    }
        DropSchema name ->
            let catalog = evalCatalog eval
                session = evalSession eval
                -- Schema names are not resolved through search_path.
                key = name
                contents = schemaContents catalog key
                schemaDeps =
                    maybe
                        Set.empty
                        (Set.fromList . catMaybes . (\state -> [definitionStatement state, definitionRestoredBy state]))
                        (Map.lookup key (catalogSchemas catalog))
                deps = schemaDeps `Set.union` schemaContentsDeps session catalog contents
                catalog1 =
                    case mode of
                        DropCascade ->
                            foldl'
                                (flip (markResolutionChange sid))
                                (removeSchemaCascade session sid key catalog)
                                (contentsRelations contents)
                        DropRestrict
                            -- A non-empty schema makes the server reject the
                            -- drop, so the catalog must stay unchanged.
                            | schemaContentsEmpty contents ->
                                catalog {catalogSchemas = Map.delete key (catalogSchemas catalog)}
                            | otherwise -> catalog
             in (addTouches (schemaTouchKeys key) (addDeps deps eval))
                    { evalCatalog = catalog1
                    }

removeRelationCascade ::
    SessionState -> StatementId -> DropMode -> RelationKey -> Catalog -> Catalog
removeRelationCascade session _sid mode = go Set.empty
    where
        go seen currentKey catalog
            | Set.member currentKey seen = catalog
            | otherwise =
                let seen' = Set.insert currentKey seen
                    dependentViews =
                        [ viewKey
                        | (viewKey, relation) <- Map.toList (catalogRelations catalog)
                        , viewKey /= currentKey
                        , relKind relation `elem` [ViewRelation, MaterializedViewRelation]
                        , currentKey `Set.member` relSources relation
                            || any
                                ( \reference ->
                                    currentKey `elem` resolveRelationKeys session catalog reference
                                )
                                (Set.toList (relUnresolvedSources relation))
                        ]
                    foreignKeys =
                        [ constraintKey
                        | (constraintKey, state, foreignKey) <- allForeignKeys catalog
                        , constraintStateTable state /= currentKey
                        , currentKey
                            `elem` resolveRelationKeys session catalog (foreignKeyParent foreignKey)
                        ]
                 in case mode of
                        DropRestrict
                            -- The server rejects the drop while dependents exist.
                            | not (null dependentViews) || not (null foreignKeys) -> catalog
                            | otherwise -> clearRelationObjects currentKey catalog
                        DropCascade ->
                            let catalog1 =
                                    foldl'
                                        (flip (go seen'))
                                        catalog
                                        dependentViews
                                catalog2 = foldl' (flip deleteConstraint) catalog1 foreignKeys
                             in clearRelationObjects currentKey catalog2

data SchemaContents = SchemaContents
    { contentsRelations :: ![RelationKey]
    , contentsSequences :: ![SequenceKey]
    , contentsIndexes :: ![IndexKey]
    , contentsTriggers :: ![TriggerKey]
    , contentsTypes :: ![Name]
    , contentsSchemas :: ![Name]
    }

schemaContents :: Catalog -> Name -> SchemaContents
schemaContents catalog schema =
    SchemaContents
        { contentsRelations =
            [ key
            | key <- Map.keys (catalogRelations catalog)
            , schemaContains schema (relationKeyName key)
            ]
        , contentsSequences =
            [ key
            | key <- Map.keys (catalogSequences catalog)
            , schemaContains schema (sequenceKeyName key)
            ]
        , contentsIndexes =
            [ key
            | key <- Map.keys (catalogIndexes catalog)
            , indexKeyInSchema schema key
            ]
        , contentsTriggers =
            [ key
            | key <- Map.keys (catalogTriggers catalog)
            , triggerKeyInSchema schema key
            ]
        , contentsTypes =
            [ name
            | name <- Map.keys (catalogTypes catalog)
            , schemaContains schema name
            ]
        , contentsSchemas =
            [ name
            | name <- Map.keys (catalogSchemas catalog)
            , schemaContains schema name
            ]
        }

schemaContentsEmpty :: SchemaContents -> Bool
schemaContentsEmpty contents =
    null (contentsRelations contents)
        && null (contentsSequences contents)
        && null (contentsIndexes contents)
        && null (contentsTriggers contents)
        && null (contentsTypes contents)
        && null (contentsSchemas contents)

schemaContentsDeps :: SessionState -> Catalog -> SchemaContents -> Set StatementId
schemaContentsDeps session catalog contents =
    Set.unions
        [ relationAllDeps session catalog relationKey
            `Set.union` relationConstraintDeps catalog relation
        | relationKey <- contentsRelations contents
        , Just relation <- [lookupRelation catalog relationKey]
        ]
        `Set.union` Set.unions
            [ Set.fromList
                ( catMaybes
                    [ sequenceDefinition state
                    , sequenceRestoredBy state
                    ]
                )
                `Set.union` sequenceWrites state
            | sequenceKey <- contentsSequences contents
            , Just state <- [Map.lookup sequenceKey (catalogSequences catalog)]
            ]
        `Set.union` Set.unions
            [ Set.fromList
                ( catMaybes
                    [ indexDefinition state
                    , indexRestoredBy state
                    ]
                )
                `Set.union` maybe
                    Set.empty
                    (relationAllDeps session catalog)
                    (indexStateTable state)
            | indexKey <- contentsIndexes contents
            , Just state <- [Map.lookup indexKey (catalogIndexes catalog)]
            ]
        `Set.union` Set.unions
            [ Set.fromList
                ( catMaybes
                    [ triggerDefinition state
                    , triggerRestoredBy state
                    ]
                )
                `Set.union` maybe
                    Set.empty
                    (relationAllDeps session catalog)
                    (triggerStateTable state)
            | triggerKey <- contentsTriggers contents
            , Just state <- [Map.lookup triggerKey (catalogTriggers catalog)]
            ]
        `Set.union` Set.unions
            [ Set.fromList
                ( catMaybes
                    [ definitionStatement state
                    , definitionRestoredBy state
                    ]
                )
            | typeName <- contentsTypes contents
            , Just state <- [Map.lookup typeName (catalogTypes catalog)]
            ]
        `Set.union` Set.unions
            [ Set.fromList
                ( catMaybes
                    [ definitionStatement state
                    , definitionRestoredBy state
                    ]
                )
            | schemaName <- contentsSchemas contents
            , Just state <- [Map.lookup schemaName (catalogSchemas catalog)]
            ]

removeSchemaCascade :: SessionState -> StatementId -> Name -> Catalog -> Catalog
removeSchemaCascade session sid schema catalog =
    let contents = schemaContents catalog schema
        catalog1 =
            foldl'
                (flip (removeRelationCascade session sid DropCascade))
                catalog
                (contentsRelations contents)
        catalog2 = foldl' (flip deleteSequence) catalog1 (contentsSequences contents)
        catalog3 = foldl' (flip deleteIndex) catalog2 (contentsIndexes contents)
        catalog4 = foldl' (flip deleteTrigger) catalog3 (contentsTriggers contents)
     in catalog4
            { catalogTypes =
                foldl' (flip Map.delete) (catalogTypes catalog4) (contentsTypes contents)
            , catalogSchemas =
                foldl' (flip Map.delete) (catalogSchemas catalog4) (contentsSchemas contents)
            }

indexKeyInSchema :: Name -> IndexKey -> Bool
indexKeyInSchema schema key =
    case key of
        NamedIndexKey name -> schemaContains schema name
        TableIndexKey relation _ -> schemaContains schema (relationKeyName relation)

triggerKeyInSchema :: Name -> TriggerKey -> Bool
triggerKeyInSchema schema key =
    case key of
        NamedTriggerKey name -> schemaContains schema name
        TableTriggerKey relation _ -> schemaContains schema (relationKeyName relation)

schemaContains :: Name -> Name -> Bool
schemaContains (Name schemaParts) (Name objectParts) =
    take (length schemaParts) objectParts == schemaParts

-- ----------------------------------------------------------------------------
-- RENAME
-- ----------------------------------------------------------------------------

processRename :: StatementId -> Eval -> RenameOp -> Eval
processRename sid eval renameOp =
    case renameOp of
        RenameRelation from to placement ->
            let session = evalSession eval
                catalog = evalCatalog eval
                keys = filter (`Map.member` catalogRelations catalog) (resolveRelationKeys session catalog from)
                stepKey current oldKey =
                    let currentCatalog = evalCatalog current
                        relation = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog oldKey)
                        deps =
                            relationAllDeps session currentCatalog oldKey
                                `Set.union` relationConstraintDeps currentCatalog relation
                                `Set.union` schemaPrefixDeps currentCatalog (relationKeyName newKey)
                        newKey =
                            RelationKey
                                (relationKeyTemp oldKey)
                                (qualifyRenameName (relationKeyName oldKey) to)
                        (catalog1, relation1) =
                            moveRelationObjects placement sid oldKey newKey relation currentCatalog
                        relation2 =
                            relation1
                                { relSchemaWrites = Set.insert sid (relSchemaWrites relation1)
                                }
                        catalog2 =
                            updateForeignKeyParents
                                session
                                currentCatalog
                                oldKey
                                newKey
                                ( updateTriggerRelations
                                    session
                                    currentCatalog
                                    oldKey
                                    newKey
                                    ( updateViewSources
                                        session
                                        currentCatalog
                                        oldKey
                                        newKey
                                        (insertRelation newKey relation2 (deleteRelation oldKey catalog1))
                                    )
                                )
                     in (addTouches (relationTouchKeys newKey ++ relationTouchKeys oldKey) (addDeps deps current))
                            { evalCatalog = catalog2
                            }
             in mutateResolved stepKey eval keys
        RenameColumnOp table from to ->
            let session = evalSession eval
                catalog = evalCatalog eval
                keys = filter (`Map.member` catalogRelations catalog) (resolveRelationKeys session catalog table)
                stepKey current key =
                    let currentCatalog = evalCatalog current
                        relation = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog key)
                        deps =
                            relationAllDeps session currentCatalog key
                                `Set.union` relationConstraintDeps currentCatalog relation
                        renameColumn column
                            | column == from = to
                            | otherwise = column
                        relation1 =
                            relation
                                { relColumns =
                                    Set.insert to (Set.delete from (relColumns relation))
                                , relColumnWrites =
                                    case Map.lookup from (relColumnWrites relation) of
                                        Nothing -> relColumnWrites relation
                                        Just writes ->
                                            Map.insertWith
                                                Set.union
                                                to
                                                writes
                                                (Map.delete from (relColumnWrites relation))
                                , relSchemaWrites = Set.insert sid (relSchemaWrites relation)
                                }
                        catalog1 =
                            currentCatalog
                                { catalogConstraints =
                                    Map.map
                                        ( \state ->
                                            let childRenamed =
                                                    if constraintStateTable state == key
                                                        then
                                                            state
                                                                { constraintValue =
                                                                    renameConstraintChildColumns
                                                                        renameColumn
                                                                        (constraintValue state)
                                                                }
                                                        else state
                                             in renameForeignKeyParentColumn
                                                    session
                                                    currentCatalog
                                                    key
                                                    renameColumn
                                                    childRenamed
                                        )
                                        (catalogConstraints currentCatalog)
                                , catalogIndexes =
                                    Map.map
                                        ( \state ->
                                            if indexStateTable state == Just key
                                                then state {indexStateColumns = map renameColumn (indexStateColumns state)}
                                                else state
                                        )
                                        (catalogIndexes currentCatalog)
                                , catalogTriggers =
                                    Map.map
                                        (renameTriggerColumns session currentCatalog key renameColumn)
                                        (catalogTriggers currentCatalog)
                                , catalogRelations =
                                    Map.map
                                        (renameSourceReadColumns session currentCatalog key renameColumn)
                                        (catalogRelations currentCatalog)
                                }
                     in (addTouches (relationTouchKeys key) (addDeps deps current))
                            { evalCatalog = insertRelation key relation1 catalog1
                            }
             in mutateResolved stepKey eval keys

moveRelationObjects ::
    IndexSchemaPlacement ->
    StatementId ->
    RelationKey ->
    RelationKey ->
    RelationState ->
    Catalog ->
    (Catalog, RelationState)
moveRelationObjects placement _sid oldKey newKey relation catalog =
    let (catalog1, newIndexes) =
            foldl' (moveIndex placement oldKey newKey) (catalog, Set.empty) (Set.toList (relIndexes relation))
        (catalog2, newTriggers) =
            foldl' (moveTrigger oldKey newKey) (catalog1, Set.empty) (Set.toList (relTriggers relation))
        (catalog3, newConstraints) =
            foldl' (moveConstraint oldKey newKey) (catalog2, Set.empty) (Set.toList (relConstraints relation))
        relation1 =
            relation
                { relIndexes = newIndexes
                , relTriggers = newTriggers
                , relConstraints = newConstraints
                }
     in (catalog3, relation1)
    where
        moveIndex schemaPlacement _old new (current, keys) indexKey =
            case Map.lookup indexKey (catalogIndexes current) of
                Nothing -> (current, keys)
                Just state ->
                    let newIndexKey =
                            case indexKey of
                                NamedIndexKey name ->
                                    case schemaPlacement of
                                        IndexTableSchema ->
                                            -- The table-schema placement moves
                                            -- an unqualified index with the
                                            -- table into the table's schema.
                                            case (nameParts name, nameParts (relationKeyName new)) of
                                                ([_], newParts)
                                                    | length newParts > 1 ->
                                                        NamedIndexKey (Name (init newParts ++ nameParts name))
                                                _ -> NamedIndexKey name
                                        IndexCurrentSchema -> NamedIndexKey name
                                TableIndexKey _ ident -> TableIndexKey new ident
                        state1 = state {indexStateTable = Just new}
                     in ( current
                            { catalogIndexes =
                                Map.insert newIndexKey state1 (Map.delete indexKey (catalogIndexes current))
                            }
                        , Set.insert newIndexKey keys
                        )
        moveTrigger _old new (current, keys) triggerKey =
            case Map.lookup triggerKey (catalogTriggers current) of
                Nothing -> (current, keys)
                Just state ->
                    let newTriggerKey =
                            case triggerKey of
                                NamedTriggerKey name -> NamedTriggerKey name
                                TableTriggerKey _ ident -> TableTriggerKey new ident
                        state1 = state {triggerStateTable = Just new}
                     in ( current
                            { catalogTriggers =
                                Map.insert newTriggerKey state1 (Map.delete triggerKey (catalogTriggers current))
                            }
                        , Set.insert newTriggerKey keys
                        )
        moveConstraint _old new (current, keys) constraintKey =
            case Map.lookup constraintKey (catalogConstraints current) of
                Nothing -> (current, keys)
                Just state ->
                    let newConstraintKey = ConstraintKey new (constraintKeyIdentity constraintKey)
                        state1 = state {constraintStateTable = new}
                     in ( current
                            { catalogConstraints =
                                Map.insert
                                    newConstraintKey
                                    state1
                                    (Map.delete constraintKey (catalogConstraints current))
                            }
                        , Set.insert newConstraintKey keys
                        )

updateViewSources :: SessionState -> Catalog -> RelationKey -> RelationKey -> Catalog -> Catalog
updateViewSources session resolutionCatalog oldKey newKey catalog =
    catalog
        { catalogRelations =
            Map.map
                ( \relation ->
                    relation
                        { relSources =
                            if Set.member oldKey (relSources relation)
                                then Set.insert newKey (Set.delete oldKey (relSources relation))
                                else relSources relation
                        , relUnresolvedSources =
                            Set.map updateUnresolved (relUnresolvedSources relation)
                        , relSourceReads =
                            map updateRead (relSourceReads relation)
                        }
                )
                (catalogRelations catalog)
        }
    where
        newReference = NameRef (relationKeyName newKey) (relationKeyTemp newKey)
        updateUnresolved reference
            | resolveRelationKeys session resolutionCatalog reference == [oldKey] =
                newReference
            | otherwise = reference
        updateRead readFact
            | oldKey `elem` resolveRelationKeys session resolutionCatalog (readSource readFact) =
                readFact {readSource = newReference}
            | otherwise = readFact

updateForeignKeyParents ::
    SessionState ->
    Catalog ->
    RelationKey ->
    RelationKey ->
    Catalog ->
    Catalog
updateForeignKeyParents session resolutionCatalog oldKey newKey catalog =
    catalog
        { catalogConstraints =
            Map.map adjust (catalogConstraints catalog)
        }
    where
        adjust state =
            case constraintValue state of
                ForeignKeyConstraint foreignKey
                    | resolveRelationKeys session resolutionCatalog (foreignKeyParent foreignKey) == [oldKey] ->
                        state
                            { constraintValue =
                                ForeignKeyConstraint
                                    foreignKey
                                        { foreignKeyParent =
                                            NameRef (relationKeyName newKey) (relationKeyTemp newKey)
                                        }
                            }
                _ -> state

renameSourceReadColumns ::
    SessionState ->
    Catalog ->
    RelationKey ->
    (Ident -> Ident) ->
    RelationState ->
    RelationState
renameSourceReadColumns session catalog key rename relation =
    relation
        { relSourceReads = map renameRead (relSourceReads relation)
        }
    where
        renameRead readFact
            | key `elem` resolveRelationKeys session catalog (readSource readFact) =
                readFact {readColumns = map rename (readColumns readFact)}
            | otherwise = readFact

renameTriggerColumns ::
    SessionState ->
    Catalog ->
    RelationKey ->
    (Ident -> Ident) ->
    TriggerState ->
    TriggerState
renameTriggerColumns session catalog key rename state =
    state
        { triggerStateWhenReads = map renameRead (triggerStateWhenReads state)
        , triggerStateBody = renameBody (triggerStateBody state)
        }
    where
        resolvesToKey reference =
            resolveRelationKeys session catalog reference == [key]
        renameRead readFact
            | resolvesToKey (readSource readFact) =
                readFact {readColumns = map rename (readColumns readFact)}
            | otherwise = readFact
        renameEvent event =
            case event of
                DmlInsert target columns
                    | resolvesToKey target -> DmlInsert target (map rename columns)
                DmlUpdate target columns
                    | resolvesToKey target -> DmlUpdate target (map rename columns)
                DmlMerge target columns
                    | resolvesToKey target -> DmlMerge target (map rename columns)
                other -> other
        renameEffect effect =
            case effect of
                TriggerDml event -> TriggerDml (renameEvent event)
                TriggerRead readFact -> TriggerRead (renameRead readFact)
                other -> other
        renameBody body =
            case body of
                TriggerInline effects -> TriggerInline (map renameEffect effects)
                other -> other

renameTriggerRelations ::
    SessionState ->
    Catalog ->
    RelationKey ->
    RelationKey ->
    TriggerState ->
    TriggerState
renameTriggerRelations session resolutionCatalog oldKey newKey state =
    state
        { triggerStateWhenReads = map renameRead (triggerStateWhenReads state)
        , triggerStateBody = renameBody (triggerStateBody state)
        }
    where
        newReference = NameRef (relationKeyName newKey) (relationKeyTemp newKey)
        resolvesToOld reference =
            resolveRelationKeys session resolutionCatalog reference == [oldKey]
        renameRead readFact
            | resolvesToOld (readSource readFact) = readFact {readSource = newReference}
            | otherwise = readFact
        renameEvent event =
            case event of
                DmlInsert target columns
                    | resolvesToOld target -> DmlInsert newReference columns
                DmlUpdate target columns
                    | resolvesToOld target -> DmlUpdate newReference columns
                DmlMerge target columns
                    | resolvesToOld target -> DmlMerge newReference columns
                DmlDelete target allRows
                    | resolvesToOld target -> DmlDelete newReference allRows
                DmlTruncate target cascade
                    | resolvesToOld target -> DmlTruncate newReference cascade
                other -> other
        renameEffect effect =
            case effect of
                TriggerDml event -> TriggerDml (renameEvent event)
                TriggerRead readFact -> TriggerRead (renameRead readFact)
                other -> other
        renameBody body =
            case body of
                TriggerInline effects -> TriggerInline (map renameEffect effects)
                other -> other

updateTriggerRelations ::
    SessionState ->
    Catalog ->
    RelationKey ->
    RelationKey ->
    Catalog ->
    Catalog
updateTriggerRelations session resolutionCatalog oldKey newKey catalog =
    catalog
        { catalogTriggers =
            Map.map
                (renameTriggerRelations session resolutionCatalog oldKey newKey)
                (catalogTriggers catalog)
        }

renameForeignKeyParentColumn ::
    SessionState ->
    Catalog ->
    RelationKey ->
    (Ident -> Ident) ->
    ConstraintState ->
    ConstraintState
renameForeignKeyParentColumn session catalog key rename state =
    case constraintValue state of
        ForeignKeyConstraint foreignKey
            | key `elem` resolveRelationKeys session catalog (foreignKeyParent foreignKey) ->
                state
                    { constraintValue =
                        ForeignKeyConstraint
                            foreignKey
                                { foreignKeyParentColumns =
                                    map rename (foreignKeyParentColumns foreignKey)
                                }
                    }
        _ -> state

-- ----------------------------------------------------------------------------
-- REFRESH
-- ----------------------------------------------------------------------------

processRefresh :: StatementId -> Eval -> RefreshOp -> Eval
processRefresh sid eval refresh =
    let session = evalSession eval
        catalog = evalCatalog eval
        keys =
            filter
                (`Map.member` catalogRelations catalog)
                (resolveRelationKeys session catalog (refreshTarget refresh))
        stepKey current key =
            let currentCatalog = evalCatalog current
                relation = fromMaybe (newRelation TableRelation) (lookupRelation currentCatalog key)
                mode = refreshSourceMode refresh
                deps = relationReadDeps session currentCatalog key ReadSchema []
                sourceReads = [readFact {readMode = mode} | readFact <- relSourceReads relation]
                evalReads = foldl' (processRead sid) current sourceReads
                relation1 =
                    relation
                        { relDataWrites = Set.singleton sid
                        , relColumnWrites = Map.empty
                        }
             in (addTouches (relationTouchKeys key) (addDeps deps evalReads))
                    { evalCatalog = insertRelation key relation1 currentCatalog
                    }
     in mutateResolved stepKey eval keys

-- ----------------------------------------------------------------------------
-- Session effects
-- ----------------------------------------------------------------------------

applySessionEffects :: StatementId -> [SessionEffect] -> Eval -> (Eval, Bool)
applySessionEffects _ effects eval =
    let eval' =
            foldl'
                ( \current effect ->
                    let session' = applySessionEffect effect (evalSession current)
                        schemaDeps =
                            case effect of
                                SetNamespace _ _ -> namespaceDeps (evalCatalog current) (effectiveNamespacePrefix session')
                                SetSearchPath _ _ -> searchPathDeps current session'
                                SetSessionUser _ _ -> searchPathDeps current session'
                                ResetSessionUser _ -> searchPathDeps current session'
                                SetCurrentUser _ _ -> searchPathDeps current session'
                                ResetCurrentUser _ -> searchPathDeps current session'
                                _ -> Set.empty
                     in (addDeps schemaDeps current)
                            { evalSession = session'
                            , evalTouches = Set.insert SessionBarrierKey (evalTouches current)
                            , evalSessionTouched = True
                            }
                )
                eval
                effects
     in (eval', evalSessionTouched eval')
    where
        searchPathDeps current session =
            Set.unions [namespaceDeps (evalCatalog current) parts | Name parts <- effectiveSearchPath session]

applySessionEffect :: SessionEffect -> SessionState -> SessionState
applySessionEffect effect session =
    case effect of
        SetVariable scope name value ->
            setVariable scope (nameText name) value session
        SetSearchPath scope path ->
            setSearchPath scope path session
        SetNamespace scope prefix ->
            case scope of
                ScopeSession -> session {sessNamespacePrefix = prefix}
                ScopeGlobal -> session
                _ -> session {sessTxNamespacePrefix = Just prefix}
        SetSessionUser scope user ->
            setSessionUser scope (Just user) session
        ResetSessionUser scope ->
            setSessionUser scope (sessAuthenticatedUser session) session
        SetCurrentUser scope user ->
            setCurrentUser scope (Just user) session
        ResetCurrentUser scope ->
            setCurrentUser scope (effectiveSessionUser session) session
        SetAutocommit scope value ->
            case scope of
                ScopeSession -> session {sessAutocommit = value}
                _ -> session
        SetForeignKeyChecks scope value ->
            case scope of
                ScopeSession -> session {sessForeignKeyChecks = value}
                _ -> session
        ResetForeignKeyChecks scope ->
            case scope of
                ScopeSession ->
                    session
                        { sessForeignKeyChecks =
                            sessInitialForeignKeyChecks session
                        }
                _ -> session
        ResetVariable scope name ->
            let key = nameText name
             in case scope of
                    ScopeSession ->
                        session {sessVariables = Map.delete key (sessVariables session)}
                    ScopeGlobal -> session
                    _ ->
                        session {sessTxVariables = Map.delete key (sessTxVariables session)}
        ResetAll scope ->
            case scope of
                ScopeSession ->
                    session
                        { sessSearchPath = []
                        , sessVariables = Map.empty
                        , sessForeignKeyChecks =
                            sessInitialForeignKeyChecks session
                        }
                ScopeGlobal -> session
                _ ->
                    session
                        { sessTxSearchPath = Nothing
                        , sessTxNamespacePrefix = Nothing
                        , sessTxCurrentUser = Nothing
                        , sessTxVariables = Map.empty
                        }

setSearchPath :: SessionScope -> SearchPathValue -> SessionState -> SessionState
setSearchPath scope path session =
    case scope of
        ScopeSession -> session {sessSearchPath = entries}
        ScopeGlobal -> session
        _ -> session {sessTxSearchPath = Just entries}
    where
        entries =
            case path of
                SearchPathDefault -> []
                SearchPathExplicit values -> values

-- | @SET SESSION AUTHORIZATION@ changes both session_user and current_user.
setSessionUser :: SessionScope -> Maybe Ident -> SessionState -> SessionState
setSessionUser scope user session =
    case scope of
        ScopeSession ->
            session
                { sessSessionUser = user
                , sessCurrentUser = user
                }
        ScopeGlobal -> session
        _ ->
            session
                { sessTxSessionUser = Just user
                , sessTxCurrentUser = Just user
                }

setCurrentUser :: SessionScope -> Maybe Ident -> SessionState -> SessionState
setCurrentUser scope user session =
    case scope of
        ScopeSession -> session {sessCurrentUser = user}
        ScopeGlobal -> session
        _ -> session {sessTxCurrentUser = Just user}

setVariable :: SessionScope -> Text -> SessionValue -> SessionState -> SessionState
setVariable scope key value session =
    let update current =
            case value of
                SessionDefault -> Map.delete key current
                _ -> Map.insert key value current
     in case scope of
            ScopeSession -> session {sessVariables = update (sessVariables session)}
            ScopeGlobal -> session
            _ -> session {sessTxVariables = update (sessTxVariables session)}

-- ----------------------------------------------------------------------------
-- Transaction frames
-- ----------------------------------------------------------------------------

createFrame :: ScanState -> Bool -> Maybe StatementId -> (TxFrame, ScanState)
createFrame state explicit begin =
    let identifier = scanNextFrameId state
        frame =
            TxFrame
                { frameId = identifier
                , frameExplicit = explicit
                , frameNesting = 1
                , frameBegin = begin
                , frameCatalog = scanCatalog state
                , frameSession = scanSession state
                , frameSavepoints = []
                }
     in (frame, state {scanNextFrameId = identifier + 1})

ensureFrame :: ScanState -> (ScanState, TxFrame)
ensureFrame state =
    case scanTransactions state of
        frame : _ -> (state, frame)
        [] ->
            let (frame, state') =
                    createFrame state (not (sessAutocommit (scanSession state))) Nothing
             in (state' {scanTransactions = [frame]}, frame)

ensureFrameForOps :: ScanState -> ScanState
ensureFrameForOps = fst . ensureFrame

finishStatement :: StatementId -> ScanState -> ScanState
finishStatement sid state =
    case scanTransactions state of
        frame : _
            | not (frameExplicit frame)
            , null (frameSavepoints frame)
            , sessAutocommit (scanSession state) ->
                finishStatement sid (commitFrame sid state)
        []
            | not (sessAutocommit (scanSession state)) ->
                let (frame, state') = createFrame state False Nothing
                 in state' {scanTransactions = [frame]}
        _ -> state

settleAutocommit :: StatementId -> ScanState -> ScanState -> ScanState
settleAutocommit sid prior next
    | sessAutocommit (scanSession prior) == sessAutocommit (scanSession next) = next
    | sessAutocommit (scanSession next) =
        case scanTransactions next of
            frame : _ | not (frameExplicit frame) -> commitFrame sid next
            _ -> next
    | otherwise =
        case scanTransactions next of
            [] ->
                let (frame, state') = createFrame next False Nothing
                 in state' {scanTransactions = [frame]}
            _ -> next

applyTxEffects :: StatementId -> [TxEffect] -> ScanState -> ScanState
applyTxEffects sid effects state = foldl' (applyTxEffect sid) state effects

applyTxEffect :: StatementId -> ScanState -> TxEffect -> ScanState
applyTxEffect sid state effect =
    case effect of
        TxBegin policy ->
            case scanTransactions state of
                [] ->
                    let (frame, state') = createFrame state True (Just sid)
                     in state' {scanTransactions = [frame]}
                frame : rest
                    | policy == BeginCommitActive ->
                        -- A `START TRANSACTION`-style begin commits the pending
                        -- transaction (including an implicit autocommit-off
                        -- frame) and starts a fresh one.
                        applyTxEffect sid (commitFrame sid state) (TxBegin BeginIgnore)
                    | not (frameExplicit frame) ->
                        state
                            { scanTransactions =
                                frame
                                    { frameExplicit = True
                                    , frameBegin = Just sid
                                    , frameNesting =
                                        if policy == BeginIgnore
                                            then frameNesting frame + 1
                                            else frameNesting frame
                                    }
                                    : rest
                            }
                    | policy == BeginIgnore ->
                        state
                            { scanTransactions =
                                frame {frameNesting = frameNesting frame + 1} : rest
                            }
                    | otherwise -> state
        TxCommit chain ->
            case scanTransactions state of
                frame : rest
                    | frameNesting frame > 1 ->
                        state
                            { scanTransactions =
                                frame {frameNesting = frameNesting frame - 1} : rest
                            }
                _ ->
                    let state' = commitFrame sid state
                     in if chain
                            then
                                let (frame, state'') = createFrame state' True (Just sid)
                                 in state'' {scanTransactions = frame : scanTransactions state'}
                            else state'
        TxRollbackAll -> rollbackFrame sid state
        TxRollbackTo name -> rollbackTo sid name state
        TxSavepoint name -> savepoint sid name state
        TxReleaseSavepoint name -> releaseSavepoint sid name state
        TxSetReadOnly value ->
            state
                { scanSession =
                    (scanSession state)
                        { sessTxVariables =
                            Map.insert
                                "transaction_read_only"
                                (SessionBool value)
                                (sessTxVariables (scanSession state))
                        }
                }

savepoint :: StatementId -> Ident -> ScanState -> ScanState
savepoint sid name state =
    let (state1, frame) = ensureFrame state
        saved =
            Savepoint
                { savepointName = name
                , savepointCatalog = scanCatalog state1
                , savepointSession = scanSession state1
                , savepointStatement = sid
                }
        frame1 = frame {frameSavepoints = saved : frameSavepoints frame}
     in state1 {scanTransactions = frame1 : drop 1 (scanTransactions state1)}

rollbackTo :: StatementId -> Ident -> ScanState -> ScanState
rollbackTo sid name state =
    case scanTransactions state of
        [] -> state
        frame : rest ->
            case break ((== name) . savepointName) (frameSavepoints frame) of
                (_, []) -> state
                (_, saved : older) ->
                    let catalog1 =
                            restoreCatalog sid (savepointCatalog saved) (scanCatalog state)
                        session1 = savepointSession saved
                        frame1 = frame {frameSavepoints = saved : older}
                     in state
                            { scanCatalog = catalog1
                            , scanSession = session1
                            , scanTransactions = frame1 : rest
                            }

releaseSavepoint :: StatementId -> Ident -> ScanState -> ScanState
releaseSavepoint sid name state =
    case scanTransactions state of
        [] -> state
        frame : rest ->
            case break ((== name) . savepointName) (frameSavepoints frame) of
                (_, []) -> state
                (_, _released : older) ->
                    let frame1 = frame {frameSavepoints = older}
                        state1 = state {scanTransactions = frame1 : rest}
                     in if not (frameExplicit frame) && null older
                            then commitFrame sid state1
                            else state1

commitFrame :: StatementId -> ScanState -> ScanState
commitFrame sid state =
    case scanTransactions state of
        [] -> state
        frame : rest ->
            let catalog1 = applyOnCommit sid (frameId frame) (scanSession state) (scanCatalog state)
                session1 = clearTransactionSession (scanSession state)
             in state
                    { scanCatalog = catalog1
                    , scanSession = session1
                    , scanTransactions = rest
                    }

rollbackFrame :: StatementId -> ScanState -> ScanState
rollbackFrame sid state =
    case scanTransactions state of
        [] -> state
        frame : rest ->
            let catalog1 = restoreCatalog sid (frameCatalog frame) (scanCatalog state)
                session1 = frameSession frame
             in state
                    { scanCatalog = catalog1
                    , scanSession = session1
                    , scanTransactions = rest
                    }

applyOnCommit :: StatementId -> Int -> SessionState -> Catalog -> Catalog
applyOnCommit sid transactionId session catalog =
    let relations =
            Map.map
                ( \relation ->
                    case relOnCommit relation of
                        Just OnCommitDeleteRows ->
                            relation
                                { relDataWrites = Set.empty
                                , relColumnWrites = Map.empty
                                }
                        _ -> relation
                )
                (catalogRelations catalog)
        dropKeys =
            [ key
            | (key, relation) <- Map.toList relations
            , relOnCommit relation == Just OnCommitDrop
            , relOnCommitFrameId relation == Just transactionId
            ]
        catalog1 = catalog {catalogRelations = relations}
     in foldl'
            (flip (removeRelationCascade session sid DropCascade))
            catalog1
            dropKeys

-- ----------------------------------------------------------------------------
-- Transactional rollback
-- ----------------------------------------------------------------------------

restoreCatalog :: StatementId -> Catalog -> Catalog -> Catalog
restoreCatalog sid snapshot current =
    Catalog
        { catalogRelations =
            restoreRelationMap sid (catalogRelations snapshot) (catalogRelations current)
        , catalogIndexes =
            restoreIndexMap sid snapshot current
        , catalogTriggers =
            restoreTriggerMap sid snapshot current
        , catalogSequences =
            restoreSequenceMap sid (catalogSequences snapshot) (catalogSequences current)
        , catalogConstraints =
            restoreConstraintMap sid snapshot current
        , catalogTypes =
            restoreDefinitionMap sid (catalogTypes snapshot) (catalogTypes current)
        , catalogSchemas =
            restoreDefinitionMap sid (catalogSchemas snapshot) (catalogSchemas current)
        , catalogNextUnnamedConstraint =
            max
                (catalogNextUnnamedConstraint snapshot)
                (catalogNextUnnamedConstraint current)
        }

restoreRelationMap ::
    StatementId ->
    Map RelationKey RelationState ->
    Map RelationKey RelationState ->
    Map RelationKey RelationState
restoreRelationMap sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet snapshot) (Map.keysSet current))
        , entry <- restoreRelationKey key
        ]
    where
        restoreRelationKey key =
            case (Map.lookup key snapshot, Map.lookup key current) of
                (Just old, Just new)
                    | nonTransactionalRelation old new -> [(key, new)]
                    | otherwise -> [(key, mark old (Just new))]
                (Just old, Nothing)
                    | nonTransactionalRelation old (newRelation (relKind old)) -> []
                    | otherwise -> [(key, mark old Nothing)]
                (Nothing, Just new)
                    | nonTransactionalRelation (newRelation (relKind new)) new -> [(key, new)]
                    | otherwise -> []
                (Nothing, Nothing) -> []
        mark old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            stripRelationRestored new /= stripRelationRestored old
             in if changed
                    then old {relRestoredBy = Just sid}
                    else old

nonTransactionalRelation :: RelationState -> RelationState -> Bool
nonTransactionalRelation left right =
    not (relTransactional left) || not (relTransactional right)

stripRelationRestored :: RelationState -> RelationState
stripRelationRestored relation = relation {relRestoredBy = Nothing}

restoreIndexMap ::
    StatementId ->
    Catalog ->
    Catalog ->
    Map IndexKey IndexState
restoreIndexMap sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet oldMap) (Map.keysSet newMap))
        , entry <- restoreIndexKey key
        ]
    where
        oldMap = catalogIndexes snapshot
        newMap = catalogIndexes current
        restoreIndexKey key =
            case (Map.lookup key oldMap, Map.lookup key newMap) of
                (Just old, Just new)
                    | nonTransactionalIndex snapshot current old new -> [(key, new)]
                    | otherwise -> [(key, mark old (Just new))]
                (Just old, Nothing)
                    | nonTransactionalIndex snapshot current old (IndexState Nothing Nothing [] Nothing) -> []
                    | otherwise -> [(key, mark old Nothing)]
                (Nothing, Just new)
                    | nonTransactionalIndex snapshot current (IndexState Nothing Nothing [] Nothing) new -> [(key, new)]
                    | otherwise -> []
                (Nothing, Nothing) -> []
        mark old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            stripIndexRestored new /= stripIndexRestored old
             in if changed
                    then old {indexRestoredBy = Just sid}
                    else old

nonTransactionalIndex :: Catalog -> Catalog -> IndexState -> IndexState -> Bool
nonTransactionalIndex snapshot current left right =
    not (indexTransactionalIn snapshot left)
        || not (indexTransactionalIn current right)

indexTransactionalIn :: Catalog -> IndexState -> Bool
indexTransactionalIn catalog state =
    case indexStateTable state of
        Nothing -> True
        Just key -> maybe True relTransactional (lookupRelation catalog key)

stripIndexRestored :: IndexState -> IndexState
stripIndexRestored state = state {indexRestoredBy = Nothing}

restoreTriggerMap ::
    StatementId ->
    Catalog ->
    Catalog ->
    Map TriggerKey TriggerState
restoreTriggerMap sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet oldMap) (Map.keysSet newMap))
        , entry <- restoreTriggerKey key
        ]
    where
        oldMap = catalogTriggers snapshot
        newMap = catalogTriggers current
        restoreTriggerKey key =
            case (Map.lookup key oldMap, Map.lookup key newMap) of
                (Just old, Just new)
                    | nonTransactionalTrigger snapshot current old new -> [(key, new)]
                    | otherwise -> [(key, mark old (Just new))]
                (Just old, Nothing)
                    | nonTransactionalTrigger snapshot current old emptyTrigger -> []
                    | otherwise -> [(key, mark old Nothing)]
                (Nothing, Just new)
                    | nonTransactionalTrigger snapshot current emptyTrigger new -> [(key, new)]
                    | otherwise -> []
                (Nothing, Nothing) -> []
        mark old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            stripTriggerRestored new /= stripTriggerRestored old
             in if changed
                    then old {triggerRestoredBy = Just sid}
                    else old

emptyTrigger :: TriggerState
emptyTrigger =
    TriggerState Nothing Nothing TriggerBefore [] [] TriggerOpaque Nothing

nonTransactionalTrigger :: Catalog -> Catalog -> TriggerState -> TriggerState -> Bool
nonTransactionalTrigger snapshot current left right =
    not (triggerTransactionalIn snapshot left)
        || not (triggerTransactionalIn current right)

triggerTransactionalIn :: Catalog -> TriggerState -> Bool
triggerTransactionalIn catalog state =
    case triggerStateTable state of
        Nothing -> True
        Just key -> maybe True relTransactional (lookupRelation catalog key)

stripTriggerRestored :: TriggerState -> TriggerState
stripTriggerRestored state = state {triggerRestoredBy = Nothing}

restoreSequenceMap ::
    StatementId ->
    Map SequenceKey SequenceState ->
    Map SequenceKey SequenceState ->
    Map SequenceKey SequenceState
restoreSequenceMap sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet snapshot) (Map.keysSet current))
        , entry <- restoreSequenceKey key
        ]
    where
        restoreSequenceKey key =
            case (Map.lookup key snapshot, Map.lookup key current) of
                (Just old, Just new)
                    | not (sequenceStateTransactional old)
                        || not (sequenceStateTransactional new) ->
                        [(key, new)]
                    | otherwise -> [(key, mark old (Just new))]
                (Just old, Nothing)
                    | not (sequenceStateTransactional old) -> []
                    | otherwise -> [(key, mark old Nothing)]
                (Nothing, Just new)
                    | not (sequenceStateTransactional new) -> [(key, new)]
                    | otherwise -> []
                (Nothing, Nothing) -> []
        mark old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            stripSequenceRestored new /= stripSequenceRestored old
             in if changed
                    then old {sequenceRestoredBy = Just sid}
                    else old

stripSequenceRestored :: SequenceState -> SequenceState
stripSequenceRestored state = state {sequenceRestoredBy = Nothing}

restoreConstraintMap ::
    StatementId ->
    Catalog ->
    Catalog ->
    Map ConstraintKey ConstraintState
restoreConstraintMap sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet oldMap) (Map.keysSet newMap))
        , entry <- restoreConstraintKey key
        ]
    where
        oldMap = catalogConstraints snapshot
        newMap = catalogConstraints current
        restoreConstraintKey key =
            case (Map.lookup key oldMap, Map.lookup key newMap) of
                (Just old, Just new)
                    | nonTransactionalConstraint snapshot current old new -> [(key, new)]
                    | otherwise -> [(key, mark old (Just new))]
                (Just old, Nothing)
                    | nonTransactionalConstraint snapshot current old emptyConstraint -> []
                    | otherwise -> [(key, mark old Nothing)]
                (Nothing, Just new)
                    | nonTransactionalConstraint snapshot current emptyConstraint new -> [(key, new)]
                    | otherwise -> []
                (Nothing, Nothing) -> []
        mark old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            stripConstraintRestored new /= stripConstraintRestored old
             in if changed
                    then old {constraintRestoredBy = Just sid}
                    else old

emptyConstraint :: ConstraintState
emptyConstraint =
    ConstraintState (RelationKey False (Name [])) Nothing (OtherConstraint Nothing) Nothing

nonTransactionalConstraint :: Catalog -> Catalog -> ConstraintState -> ConstraintState -> Bool
nonTransactionalConstraint snapshot current left right =
    not (constraintTransactionalIn snapshot left)
        || not (constraintTransactionalIn current right)

constraintTransactionalIn :: Catalog -> ConstraintState -> Bool
constraintTransactionalIn catalog state =
    maybe True relTransactional (lookupRelation catalog (constraintStateTable state))

stripConstraintRestored :: ConstraintState -> ConstraintState
stripConstraintRestored state = state {constraintRestoredBy = Nothing}

restoreDefinitionMap ::
    StatementId ->
    Map Name DefinitionState ->
    Map Name DefinitionState ->
    Map Name DefinitionState
restoreDefinitionMap sid snapshot current =
    Map.fromList
        [ entry
        | key <- Set.toList (Set.union (Map.keysSet snapshot) (Map.keysSet current))
        , entry <- restoreDefinitionKey key
        ]
    where
        restoreDefinitionKey key =
            case (Map.lookup key snapshot, Map.lookup key current) of
                (Just old, Just new) -> [(key, mark old (Just new))]
                (Just old, Nothing) -> [(key, mark old Nothing)]
                (Nothing, _) -> []
        mark old currentState =
            let changed =
                    case currentState of
                        Nothing -> True
                        Just new ->
                            stripDefinitionRestored new /= stripDefinitionRestored old
             in if changed
                    then old {definitionRestoredBy = Just sid}
                    else old

stripDefinitionRestored :: DefinitionState -> DefinitionState
stripDefinitionRestored state = state {definitionRestoredBy = Nothing}

-- ----------------------------------------------------------------------------
-- Statement barrier finalization
-- ----------------------------------------------------------------------------

finalizeStatement ::
    StatementId ->
    Eval ->
    Bool ->
    ScanState ->
    ScanState ->
    (Set StatementId, ScanState, Int, Bool, Int)
finalizeStatement sid eval sessionTouched priorState resolutionState =
    let declared = evalBarriers eval
        resolutionSession = scanSession resolutionState
        resolutionCatalog = scanCatalog resolutionState
        barrierKeys =
            concatMap
                (barrierKeysForScope resolutionSession resolutionCatalog . barrierScope)
                declared
        touchedKeys = Set.toList (evalTouches eval)
        baseDeps =
            lastBarrierDeps
                priorState
                [GlobalBarrierKey, SessionBarrierKey, TransactionBarrierKey]
        touchDeps = lastBarrierDeps priorState touchedKeys
        hasGlobalFence = Map.member GlobalBarrierKey (scanLastBarriers priorState)
        declaredDeps =
            Set.unions
                [ if key == GlobalBarrierKey
                    then if hasGlobalFence then Set.empty else scanPriorStatements priorState
                    else touchersForBarrier priorState key
                | key <- barrierKeys
                ]
        barrierDeps =
            Set.unions [baseDeps, touchDeps, declaredDeps]
        objectDeps = evalDeps eval
        allDeps = Set.delete sid (Set.union objectDeps barrierDeps)
        barrierEdges =
            Set.size (Set.delete sid (Set.difference barrierDeps objectDeps))
        sessionKeys = [SessionBarrierKey | sessionTouched]
        -- NOTE: unknown effects serialize the remaining script; recover
        -- precision only when those effects can be represented in the catalog.
        inheritedGlobal = [GlobalBarrierKey | hasGlobalFence]
        registrationKeys = barrierKeys ++ sessionKeys ++ inheritedGlobal
        lastBarriers =
            foldl'
                (\current key -> Map.insert key sid current)
                (scanLastBarriers priorState)
                registrationKeys
        allTouched =
            Set.union (evalTouches eval) (Set.fromList registrationKeys)
        touchers =
            foldl'
                (\current key -> Map.insertWith Set.union key (Set.singleton sid) current)
                (scanTouchers priorState)
                (Set.toList allTouched)
        state' =
            resolutionState
                { scanLastBarriers = lastBarriers
                , scanTouchers = touchers
                , scanPriorStatements = Set.insert sid (scanPriorStatements priorState)
                }
        hasBarrier = not (null declared) || sessionTouched
        barrierActions = length declared + if sessionTouched then 1 else 0
     in (allDeps, state', barrierEdges, hasBarrier, barrierActions)
