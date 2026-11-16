-- | Reads, data writes, triggers, sequences, foreign-key cascades, refresh.
module Occam.Dependency.Write (
    processRead,
    sequenceReadDeps,
    DmlTag (..),
    dmlTagFor,
    eventMatches,
    matchingTriggers,
    processDataWrite,
    writeRelation,
    addViewWriteBarriers,
    transitiveSources,
    expandTrigger,
    processTriggerEffect,
    processDmlEvent,
    processSequenceWrite,
    applyForeignKeys,
    applyChildChecks,
    applyParentActions,
    applyWriteState,
    processRefresh,
) where

import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core (
    BarrierReason (..),
    BarrierScope (..),
    ConflictAction (..),
    DataWrite (..),
    DeleteScope (..),
    DmlEvent (..),
    ForeignKey (..),
    Ident,
    NameRef (..),
    ReadFact (..),
    ReadMode (..),
    ReferentialAction (..),
    RefreshOp (..),
    RelationKind (..),
    ScopedBarrier (..),
    SequenceWrite (..),
    StatementId,
    TriggerBody (..),
    TriggerEffect (..),
    TriggerEvent (..),
    TriggerTiming (..),
    TruncateBehavior (..),
    TruncateIdentity (..),
    WriteKind (..),
    nameText,
 )
import Occam.Dependency.Barrier (relationTouchKeys, sequenceTouchKeys, triggerTouchKeys)
import Occam.Dependency.Query (
    allForeignKeys,
    referencedParentColumns,
    relationReadDeps,
    relationWriteDeps,
 )
import Occam.Dependency.Resolve (relationFallbackKey, resolveRelationKeys, resolveSequenceKeys)
import Occam.Dependency.Session (applySessionEffects)
import Occam.Dependency.State (
    BarrierKey (..),
    Catalog (..),
    ConstraintState (..),
    Eval (..),
    RelationKey (..),
    RelationState (..),
    SequenceKey,
    SequenceState (..),
    SessionState (..),
    TriggerKey,
    TriggerState (..),
    addBarrier,
    addDeps,
    addTouch,
    addTouches,
    insertRelation,
    lookupRelation,
    mutateResolved,
    newRelation,
    writeSignature,
 )

-- Write-state application
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

-- Reads
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

-- Trigger matching
data DmlTag
    = TagInsert
    | TagUpdate ![Ident]
    | TagDelete
    | TagMerge
    | TagTruncate
    deriving (Eq, Show)

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

-- Data writes
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

-- Triggers and sequences
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

-- Foreign keys
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

-- Refresh
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
