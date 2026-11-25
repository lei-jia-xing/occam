-- | Binary decoders for SQL semantic facts, catalog operations, and session state.
module Occam.Parser.Binary.Semantics (
    getSemanticFacts,
    getSessionInit,
) where

import Data.Map.Strict qualified as Map
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
    IndexDef (..),
    IndexRef (..),
    IndexRole (..),
    IndexSchemaPlacement (..),
    IndexUniqueness (..),
    ObjectRef (..),
    OnCommitAction (..),
    PartitionLink (..),
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
    SourceColumn (..),
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
    UpdateScope (..),
    ViewColumnLineage (..),
    ViewDef (..),
    WriteKind (..),
 )
import Occam.Parser.Binary.Primitives (
    Decoder,
    getBool,
    getIdent,
    getInt64,
    getName,
    getNameRef,
    getOptional,
    getText,
    getVec,
    getWord8,
 )

getSearchPathEntry :: Decoder SearchPathEntry
getSearchPathEntry = do
    tag <- getWord8
    case tag of
        0 -> pure SearchPathCurrentUser
        1 -> SearchPathName <$> getName
        _ -> fail ("invalid SearchPathEntry tag: " ++ show tag)

getSearchPathValue :: Decoder SearchPathValue
getSearchPathValue = do
    tag <- getWord8
    case tag of
        0 -> pure SearchPathDefault
        1 -> SearchPathExplicit <$> getVec getSearchPathEntry
        _ -> fail ("invalid SearchPathValue tag: " ++ show tag)

getSessionInit :: Decoder SessionInit
getSessionInit = do
    nsPrefix <- getVec getIdent
    searchPath <- getVec getSearchPathEntry
    currentUser <- getOptional getIdent
    autocommit <- getBool
    fkeys <- getBool
    varPairs <- getVec ((,) <$> getText <*> getText)
    pure
        SessionInit
            { sessionNamespacePrefix = nsPrefix
            , sessionSearchPath = searchPath
            , sessionCurrentUser = currentUser
            , sessionAutocommit = autocommit
            , sessionForeignKeyChecks = fkeys
            , sessionVariables = Map.fromList varPairs
            }

getReadMode :: Decoder ReadMode
getReadMode = do
    code <- getWord8
    case code of
        0 -> pure ReadData
        1 -> pure ReadSchema
        2 -> pure ReadRowCount
        3 -> pure ReadRowExistence
        _ -> fail ("invalid ReadMode tag: " ++ show code)

getReadFact :: Decoder ReadFact
getReadFact = ReadFact <$> getNameRef <*> getReadMode <*> getVec getIdent <*> getBool

getRelationKind :: Decoder RelationKind
getRelationKind = do
    code <- getWord8
    case code of
        0 -> pure TableRelation
        1 -> pure ViewRelation
        2 -> pure MaterializedViewRelation
        _ -> fail ("invalid RelationKind tag: " ++ show code)

getReferentialAction :: Decoder ReferentialAction
getReferentialAction = do
    code <- getWord8
    case code of
        0 -> pure NoAction
        1 -> pure Restrict
        2 -> pure Cascade
        3 -> pure SetNull
        4 -> pure SetDefault
        _ -> fail ("invalid ReferentialAction tag: " ++ show code)

getOnCommitAction :: Decoder OnCommitAction
getOnCommitAction = do
    code <- getWord8
    case code of
        0 -> pure OnCommitDeleteRows
        1 -> pure OnCommitDrop
        _ -> fail ("invalid OnCommitAction tag: " ++ show code)

getIndexSchemaPlacement :: Decoder IndexSchemaPlacement
getIndexSchemaPlacement = do
    code <- getWord8
    case code of
        0 -> pure IndexTableSchema
        1 -> pure IndexCurrentSchema
        _ -> fail ("invalid IndexSchemaPlacement tag: " ++ show code)

getForeignKey :: Decoder ForeignKey
getForeignKey =
    ForeignKey
        <$> getOptional getIdent
        <*> getVec getIdent
        <*> getNameRef
        <*> getVec getIdent
        <*> getReferentialAction
        <*> getReferentialAction
        <*> getBool
        <*> getBool

getConstraint :: Decoder Constraint
getConstraint = do
    tag <- getWord8
    case tag of
        0 -> PrimaryKeyConstraint <$> getOptional getIdent <*> getVec getIdent
        1 -> UniqueConstraint <$> getOptional getIdent <*> getVec getIdent
        2 -> ForeignKeyConstraint <$> getForeignKey
        3 ->
            CheckConstraint
                <$> getOptional getIdent
                <*> getVec getIdent
                <*> ((\notValid -> if notValid then ConstraintNotValid else ConstraintValid) <$> getBool)
        4 ->
            IndexConstraint
                <$> getOptional getIdent
                <*> getVec getIdent
                <*> ((\unique -> if unique then IndexUnique else IndexNonUnique) <$> getBool)
        5 ->
            UsingIndexConstraint
                <$> getOptional getIdent
                <*> getIdent
                <*> ((\primary -> if primary then PrimaryIndex else SecondaryIndex) <$> getBool)
                <*> getIndexSchemaPlacement
        6 -> OtherConstraint <$> getOptional getIdent
        _ -> fail ("invalid Constraint tag: " ++ show tag)

getColumnDef :: Decoder ColumnDef
getColumnDef =
    ColumnDef
        <$> getIdent
        <*> getBool
        <*> getBool
        <*> getBool
        <*> getVec getForeignKey

getTableDef :: Decoder TableDef
getTableDef =
    TableDef
        <$> getName
        <*> getBool
        <*> getBool
        <*> getVec getColumnDef
        <*> getVec getConstraint
        <*> getVec getReadFact
        <*> getOptional getNameRef
        <*> getOptional getNameRef
        <*> getVec getNameRef
        <*> getOptional getOnCommitAction
        <*> getBool
        <*> getBool

getSourceColumn :: Decoder SourceColumn
getSourceColumn = SourceColumn <$> getNameRef <*> getIdent

getViewColumnLineage :: Decoder ViewColumnLineage
getViewColumnLineage = ViewColumnLineage <$> getIdent <*> getOptional (getVec getSourceColumn)

getViewDef :: Decoder ViewDef
getViewDef =
    ViewDef
        <$> getName
        <*> getBool
        <*> getBool
        <*> getVec getIdent
        <*> getVec getReadFact
        <*> getBool
        <*> getBool
        <*> getVec getViewColumnLineage
        <*> getVec getSourceColumn

getIndexRef :: Decoder IndexRef
getIndexRef = do
    tag <- getWord8
    case tag of
        0 -> NamedIndex <$> getName
        1 -> TableIndex <$> getNameRef <*> getIdent
        _ -> fail ("invalid IndexRef tag: " ++ show tag)

getIndexDef :: Decoder IndexDef
getIndexDef =
    IndexDef
        <$> getIndexRef
        <*> getNameRef
        <*> getVec getIdent
        <*> getBool
        <*> getBool
        <*> getBool
        <*> getBool
        <*> getIndexSchemaPlacement

getTriggerRef :: Decoder TriggerRef
getTriggerRef = do
    tag <- getWord8
    case tag of
        0 -> NamedTrigger <$> getName
        1 -> TableTrigger <$> getNameRef <*> getIdent
        _ -> fail ("invalid TriggerRef tag: " ++ show tag)

getConstraintRef :: Decoder ConstraintRef
getConstraintRef = ConstraintRef <$> getNameRef <*> getIdent

getTriggerTiming :: Decoder TriggerTiming
getTriggerTiming = do
    code <- getWord8
    case code of
        0 -> pure TriggerBefore
        1 -> pure TriggerAfter
        2 -> pure TriggerInsteadOf
        _ -> fail ("invalid TriggerTiming tag: " ++ show code)

getTriggerEvent :: Decoder TriggerEvent
getTriggerEvent = do
    tag <- getWord8
    case tag of
        0 -> pure TriggerInsert
        1 -> TriggerUpdate <$> getVec getIdent
        2 -> pure TriggerDelete
        3 -> pure TriggerTruncate
        _ -> fail ("invalid TriggerEvent tag: " ++ show tag)

getDmlEvent :: Decoder DmlEvent
getDmlEvent = do
    tag <- getWord8
    case tag of
        0 -> DmlInsert <$> getNameRef <*> getVec getIdent
        1 -> DmlUpdate <$> getNameRef <*> getVec getIdent
        2 -> DmlMerge <$> getNameRef <*> getVec getIdent
        3 ->
            DmlDelete
                <$> getNameRef
                <*> ((\allRows -> if allRows then DeleteAllRows else DeleteFilteredRows) <$> getBool)
        4 ->
            DmlTruncate
                <$> getNameRef
                <*> ((\cascade -> if cascade then TruncateCascade else TruncateRestrict) <$> getBool)
        _ -> fail ("invalid DmlEvent tag: " ++ show tag)

getSequenceWrite :: Decoder SequenceWrite
getSequenceWrite = SequenceWrite <$> getNameRef <*> getBool

getSessionScope :: Decoder SessionScope
getSessionScope = do
    code <- getWord8
    case code of
        0 -> pure ScopeSession
        1 -> pure ScopeTransaction
        2 -> pure ScopeLocal
        3 -> pure ScopeGlobal
        _ -> fail ("invalid SessionScope tag: " ++ show code)

getSessionValue :: Decoder SessionValue
getSessionValue = do
    tag <- getWord8
    case tag of
        0 -> SessionText <$> getText
        1 -> SessionBool <$> getBool
        2 -> SessionInteger <$> getInt64
        3 -> SessionNames <$> getVec getName
        4 -> pure SessionDefault
        _ -> fail ("invalid SessionValue tag: " ++ show tag)

getSessionEffect :: Decoder SessionEffect
getSessionEffect = do
    tag <- getWord8
    case tag of
        0 -> SetVariable <$> getSessionScope <*> getName <*> getSessionValue
        1 -> SetSearchPath <$> getSessionScope <*> getSearchPathValue
        2 -> SetNamespace <$> getSessionScope <*> getVec getIdent
        3 -> SetSessionUser <$> getSessionScope <*> getIdent
        4 -> ResetSessionUser <$> getSessionScope
        5 -> SetCurrentUser <$> getSessionScope <*> getIdent
        6 -> ResetCurrentUser <$> getSessionScope
        7 -> SetAutocommit <$> getSessionScope <*> getBool
        8 -> SetForeignKeyChecks <$> getSessionScope <*> getBool
        9 -> SetSqliteForeignKeys <$> getBool
        10 -> ResetForeignKeyChecks <$> getSessionScope
        11 -> ResetVariable <$> getSessionScope <*> getName
        12 -> ResetAll <$> getSessionScope
        _ -> fail ("invalid SessionEffect tag: " ++ show tag)

getBeginPolicy :: Decoder BeginPolicy
getBeginPolicy = do
    code <- getWord8
    case code of
        0 -> pure BeginError
        1 -> pure BeginWarn
        2 -> pure BeginCommitActive
        3 -> pure BeginIgnore
        _ -> fail ("invalid BeginPolicy tag: " ++ show code)

getTxEffect :: Decoder TxEffect
getTxEffect = do
    tag <- getWord8
    case tag of
        0 -> TxBegin <$> getBeginPolicy
        1 -> TxCommit <$> getBool
        2 -> pure TxRollbackAll
        3 -> TxRollbackTo <$> getIdent
        4 -> TxSavepoint <$> getIdent
        5 -> TxReleaseSavepoint <$> getIdent
        6 -> TxSetReadOnly <$> getBool
        _ -> fail ("invalid TxEffect tag: " ++ show tag)

getObjectRef :: Decoder ObjectRef
getObjectRef = do
    tag <- getWord8
    case tag of
        0 -> ObjectRelation <$> getNameRef
        1 -> ObjectIndex <$> getIndexRef
        2 -> ObjectTrigger <$> getTriggerRef
        3 -> ObjectSequence <$> getNameRef
        4 -> ObjectType <$> getName
        5 -> ObjectSchema <$> getName
        6 -> ObjectConstraint <$> getConstraintRef
        _ -> fail ("invalid ObjectRef tag: " ++ show tag)

getBarrierScope :: Decoder BarrierScope
getBarrierScope = do
    tag <- getWord8
    case tag of
        0 -> pure GlobalScope
        1 -> RelationScope <$> getNameRef
        2 -> SchemaScope <$> getName
        3 -> ObjectScope <$> getObjectRef
        4 -> pure SessionStateScope
        5 -> pure TransactionStateScope
        6 -> UserVariableScope <$> getIdent
        _ -> fail ("invalid BarrierScope tag: " ++ show tag)

getBarrierReason :: Decoder BarrierReason
getBarrierReason = do
    code <- getWord8
    case code of
        0 -> pure SyntaxErrorBarrier
        1 -> pure UnknownStatementBarrier
        2 -> pure UnknownRelationBarrier
        3 -> pure UnknownDDLBarrier
        4 -> pure UnknownDMLBarrier
        5 -> pure UnknownDropBarrier
        6 -> pure UnknownAlterBarrier
        7 -> pure UnknownViewBarrier
        8 -> pure UnknownIndexBarrier
        9 -> pure UnknownTriggerBarrier
        10 -> pure UnknownTruncateBarrier
        11 -> pure UnknownSessionEffectBarrier
        12 -> pure UnsupportedExpressionBarrier
        13 -> pure UnsupportedConflictClauseBarrier
        14 -> pure UnsupportedTriggerBodyBarrier
        15 -> pure UnsupportedTransactionBarrier
        16 -> pure UnresolvedViewSourceBarrier
        17 -> pure WriteThroughViewBarrier
        18 -> pure OpaqueTriggerBodyBarrier
        19 -> pure UnmodeledRelationSourceBarrier
        _ -> fail ("invalid BarrierReason tag: " ++ show code)

getScopedBarrier :: Decoder ScopedBarrier
getScopedBarrier = ScopedBarrier <$> getBarrierScope <*> getBarrierReason <*> getOptional getText

getTriggerEffect :: Decoder TriggerEffect
getTriggerEffect = do
    tag <- getWord8
    case tag of
        0 -> TriggerDml <$> getDmlEvent
        1 -> TriggerRead <$> getReadFact
        2 -> TriggerWriteSequence <$> getSequenceWrite
        3 -> TriggerSession <$> getSessionEffect
        4 -> TriggerBarrier <$> getScopedBarrier
        _ -> fail ("invalid TriggerEffect tag: " ++ show tag)

getTriggerBody :: Decoder TriggerBody
getTriggerBody = do
    tag <- getWord8
    case tag of
        0 -> TriggerInline <$> getVec getTriggerEffect
        1 -> TriggerFunction <$> getName <*> getVec getText
        2 -> pure TriggerOpaque
        _ -> fail ("invalid TriggerBody tag: " ++ show tag)

getTriggerDef :: Decoder TriggerDef
getTriggerDef =
    TriggerDef
        <$> getTriggerRef
        <*> getNameRef
        <*> getTriggerTiming
        <*> getVec getTriggerEvent
        <*> getBool
        <*> getVec getReadFact
        <*> getTriggerBody
        <*> getOptional getNameRef
        <*> getBool
        <*> getBool
        <*> getBool

getConflictAction :: Decoder ConflictAction
getConflictAction = do
    code <- getWord8
    case code of
        0 -> pure ConflictError
        1 -> pure ConflictIgnore
        2 -> pure ConflictReplace
        3 -> pure ConflictUpdate
        _ -> fail ("invalid ConflictAction tag: " ++ show code)

getWriteKind :: Decoder WriteKind
getWriteKind = do
    tag <- getWord8
    case tag of
        0 -> InsertWrite <$> getConflictAction
        1 ->
            UpdateWrite . (\allRows -> if allRows then UpdateAllRows else UpdateFilteredRows) <$> getBool
        2 ->
            DeleteWrite . (\allRows -> if allRows then DeleteAllRows else DeleteFilteredRows) <$> getBool
        3 -> pure MergeWrite
        4 ->
            (TruncateWrite . (\cascade -> if cascade then TruncateCascade else TruncateRestrict) <$> getBool)
                <*> ((\restart -> if restart then RestartIdentity else ContinueIdentity) <$> getBool)
        _ -> fail ("invalid WriteKind tag: " ++ show tag)

getDataWrite :: Decoder DataWrite
getDataWrite =
    DataWrite
        <$> getNameRef
        <*> getWriteKind
        <*> getVec getIdent
        <*> getVec getIdent
        <*> getVec getReadFact
        <*> getBool

getAlterChange :: Decoder AlterChange
getAlterChange = do
    tag <- getWord8
    case tag of
        0 -> AddColumn <$> getColumnDef <*> getVec getConstraint
        1 -> DropColumn <$> getIdent <*> getBool
        2 -> AddConstraint <$> getConstraint
        3 -> DropConstraint <$> getIdent <*> getBool
        4 -> RenameConstraint <$> getIdent <*> getIdent
        5 -> ValidateConstraint <$> getIdent
        6 -> DropIndexChange <$> getIdent
        7 -> AlterColumn <$> getIdent <*> getText <*> getVec getIdent <*> getBool
        8 -> OtherChange <$> getText
        _ -> fail ("invalid AlterChange tag: " ++ show tag)

getAlterTable :: Decoder AlterTable
getAlterTable = AlterTable <$> getNameRef <*> getVec getAlterChange

getPartitionLink :: Decoder PartitionLink
getPartitionLink = PartitionLink <$> getNameRef <*> getNameRef

getDropMode :: Decoder DropMode
getDropMode = do
    code <- getWord8
    case code of
        0 -> pure DropRestrict
        1 -> pure DropCascade
        _ -> fail ("invalid DropMode tag: " ++ show code)

getDropObject :: Decoder DropObject
getDropObject = do
    tag <- getWord8
    case tag of
        0 -> DropRelation <$> getNameRef <*> getRelationKind
        1 -> DropIndex <$> getIndexRef
        2 -> DropTrigger <$> getTriggerRef
        3 -> DropSequence <$> getNameRef
        4 -> DropType <$> getName
        5 -> DropSchema <$> getName
        _ -> fail ("invalid DropObject tag: " ++ show tag)

getDropOp :: Decoder DropOp
getDropOp = DropOp <$> getVec getDropObject <*> getDropMode

getRenameOp :: Decoder RenameOp
getRenameOp = do
    tag <- getWord8
    case tag of
        0 -> RenameRelation <$> getNameRef <*> getName <*> getIndexSchemaPlacement
        1 -> RenameColumnOp <$> getNameRef <*> getIdent <*> getIdent
        _ -> fail ("invalid RenameOp tag: " ++ show tag)

getRefreshOp :: Decoder RefreshOp
getRefreshOp = RefreshOp <$> getNameRef <*> getReadMode

getCatalogOp :: Decoder CatalogOp
getCatalogOp = do
    tag <- getWord8
    case tag of
        0 -> DefineTableOp <$> getTableDef
        1 -> DefineViewOp <$> getViewDef
        2 -> DefineIndexOp <$> getIndexDef
        3 -> DefineTriggerOp <$> getTriggerDef
        4 -> DefineSequenceOp <$> (SequenceDef <$> getName <*> getBool <*> getBool)
        5 -> DefineTypeOp . TypeDef <$> getName
        6 -> DefineSchemaOp . SchemaDef <$> getName
        7 -> ReadOp <$> getReadFact
        8 -> WriteDataOp <$> getDataWrite
        9 -> WriteSequenceOp <$> getSequenceWrite
        10 -> AlterTableOp <$> getAlterTable
        11 -> AttachPartitionOp <$> getPartitionLink
        12 -> DetachPartitionOp <$> getPartitionLink
        13 -> AttachInheritOp <$> getPartitionLink
        14 -> DetachInheritOp <$> getPartitionLink
        15 -> DropCatalogOp <$> getDropOp
        16 -> RenameOp <$> getRenameOp
        17 -> RefreshCatalogOp <$> getRefreshOp
        _ -> fail ("invalid CatalogOp tag: " ++ show tag)

{-# INLINE getSemanticFacts #-}
getSemanticFacts :: Decoder SemanticFacts
getSemanticFacts =
    SemanticFacts
        <$> getVec getCatalogOp
        <*> getVec getSessionEffect
        <*> getVec getTxEffect
        <*> getVec getTxEffect
        <*> getVec getScopedBarrier
