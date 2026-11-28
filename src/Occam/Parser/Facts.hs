-- | Diagnostics for the hidden @--dump-facts@ CLI.
module Occam.Parser.Facts (
    factDumpJson,
) where

import Data.Aeson (Value, object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Core.Schema (
    AlterChange (..),
    AlterTable (..),
    ConflictAction (..),
    ConstraintRef (..),
    DataWrite (..),
    DmlEvent (..),
    DropMode (..),
    DropObject (..),
    DropOp (..),
    Ident (..),
    IndexDef (..),
    IndexRef (..),
    Name,
    NameRef (..),
    PartitionLink (..),
    ReadFact (..),
    ReadMode (..),
    RefreshOp (..),
    RelationKind (..),
    RenameOp (..),
    SchemaDef (..),
    SequenceDef (..),
    SequenceWrite (..),
    TableDef (..),
    TriggerRef (..),
    TypeDef (..),
    ViewDef (..),
    WriteKind (..),
    nameText,
 )
import Occam.Core.Semantics (
    BarrierReason (..),
    BarrierScope (..),
    CatalogOp (..),
    ObjectRef (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    SessionEffect (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEffect (..),
    TxEffect (..),
 )
import Occam.Core.Types (Span (..), Statement (..), StatementId)
import Occam.Dependency.Enforcement (cascadeWrites)
import Occam.Facts (StatementFacts (..), StatementUnit (..))
import Occam.Parser.Types (ParsedSQL (..), SQLDialect (..), dialectName)

factDumpJson :: SQLDialect -> ParsedSQL -> Value
factDumpJson dialect parsed =
    object
        [ "dialect" .= dialectName dialect
        , "statements" .= zipWith (statementFactsJson extras) [0 :: Int ..] (parsedUnits parsed)
        ]
    where
        extras = cascadeWrites (parsedInitialSession parsed) (parsedUnits parsed)

statementFactsJson :: Map StatementId [DataWrite] -> Int -> StatementUnit -> Value
statementFactsJson extras idx unit =
    object
        [ "index" .= idx
        , "start" .= startByte
        , "end" .= endByte
        , "parse_error" .= unitParseError unit
        , "reads" .= readsJson
        , "writes" .= writesJson
        , "ddl" .= ddlJson
        , "barriers" .= map barrierJson (factBarriers semantics)
        , "session" .= map sessionEffectName (factSession semantics)
        , "pre_tx" .= map txEffectName (factPreTx semantics)
        , "post_tx" .= map txEffectName (factPostTx semantics)
        , "trigger_effects" .= triggerEffectsJson ops
        ]
    where
        Span startByte endByte = stmtSpan (unitStatement unit)
        semantics = maybe (SemanticFacts [] [] [] [] []) factSemantics (unitFacts unit)
        ops = factOps semantics
        cascadeExtras = Map.findWithDefault [] (stmtId (unitStatement unit)) extras
        readsJson = map readFactJson (concatMap opReads ops)
        writesJson = concatMap opWrites ops ++ map dataWriteJson cascadeExtras
        ddlJson = concatMap opDdl ops

-- | Reads evaluated when the statement executes directly.
opReads :: CatalogOp -> [ReadFact]
opReads (ReadOp r) = [r]
opReads (WriteDataOp w) = dataWriteReads w
opReads (DefineTableOp t) = tableSources t
opReads (DefineViewOp v) = viewSources v
opReads _ = []

opWrites :: CatalogOp -> [Value]
opWrites (WriteDataOp w) = [dataWriteJson w]
opWrites (RefreshCatalogOp r) =
    [ object
        [ "target" .= nameText (nameRefName (refreshTarget r))
        , "kind" .= ("refresh" :: T.Text)
        , "columns" .= ([] :: [T.Text])
        , "unqualified_columns" .= ([] :: [T.Text])
        ]
    ]
opWrites _ = []

opDdl :: CatalogOp -> [Value]
opDdl (DefineTableOp t) = [ddlOpJson "create-table" (nameText (tableName t))]
opDdl (DefineViewOp v) =
    [ ddlOpJson
        (if viewMaterialized v then "create-materialized-view" else "create-view")
        (nameText (viewName v))
    ]
opDdl (DefineIndexOp i) =
    [ object
        [ "op" .= ("create-index" :: T.Text)
        , "target" .= nameText (nameRefName (indexTable i))
        , "name" .= indexRefName (indexRef i)
        ]
    ]
opDdl (DefineTriggerOp t) =
    [ object
        [ "op" .= ("create-trigger" :: T.Text)
        , "target" .= nameText (nameRefName (triggerTable t))
        , "name" .= triggerRefName (triggerRef t)
        ]
    ]
opDdl (DefineSequenceOp s) = [ddlOpJson "create-sequence" (nameText (sequenceName s))]
opDdl (DefineTypeOp t) = [ddlOpJson "create-type" (nameText (typeName t))]
opDdl (DefineSchemaOp s) = [ddlOpJson "create-schema" (nameText (schemaName s))]
opDdl (AlterTableOp a) =
    [ object
        [ "op" .= ("alter-table" :: T.Text)
        , "target" .= nameText (nameRefName (alterTableTarget a))
        , "changes" .= map alterChangeName (alterTableChanges a)
        ]
    ]
opDdl (AttachPartitionOp l) = [ddlOpJson "attach-partition" (nameText (nameRefName (linkChild l)))]
opDdl (DetachPartitionOp l) = [ddlOpJson "detach-partition" (nameText (nameRefName (linkChild l)))]
opDdl (DropCatalogOp d) = map (dropObjectJson (dropMode d)) (dropObjects d)
opDdl (RenameOp r) = [renameJson r]
opDdl (WriteSequenceOp s) = [ddlOpJson "sequence-write" (nameText (nameRefName (sequenceWriteTarget s)))]
opDdl _ = []

ddlOpJson :: T.Text -> T.Text -> Value
ddlOpJson op target = object ["op" .= op, "target" .= target]

readFactJson :: ReadFact -> Value
readFactJson r =
    object
        [ "table" .= nameText (nameRefName (readSource r))
        , "temp" .= nameRefTemp (readSource r)
        , "columns" .= map identText (readColumns r)
        , "mode" .= case readMode r of
            ReadData -> "data" :: T.Text
            ReadSchema -> "schema"
            ReadRowCount -> "row_count"
            ReadRowExistence -> "row_existence"
        ]

dataWriteJson :: DataWrite -> Value
dataWriteJson w =
    object
        [ "target" .= nameText (nameRefName (dataWriteTarget w))
        , "kind" .= writeKindName (dataWriteKind w)
        , "columns" .= map identText (dataWriteColumns w)
        , "unqualified_columns" .= map identText (dataWriteUnqualifiedColumns w)
        ]

writeKindName :: WriteKind -> T.Text
writeKindName (InsertWrite ConflictError) = "insert"
writeKindName (InsertWrite ConflictIgnore) = "insert-or-ignore"
writeKindName (InsertWrite ConflictReplace) = "insert-or-replace"
writeKindName (InsertWrite ConflictUpdate) = "insert-or-update"
writeKindName (UpdateWrite _) = "update"
writeKindName (DeleteWrite _) = "delete"
writeKindName MergeWrite = "merge"
writeKindName (TruncateWrite _ _) = "truncate"

alterChangeName :: AlterChange -> T.Text
alterChangeName (AddColumn _ _) = "add-column"
alterChangeName (DropColumn _ _) = "drop-column"
alterChangeName (AddConstraint _) = "add-constraint"
alterChangeName (DropConstraint _ _) = "drop-constraint"
alterChangeName (RenameConstraint _ _) = "rename-constraint"
alterChangeName (ValidateConstraint _) = "validate-constraint"
alterChangeName (DropIndexChange _) = "drop-index"
alterChangeName (AlterColumn {}) = "alter-column"
alterChangeName (OtherChange detail) = "other:" <> detail

dropObjectJson :: DropMode -> DropObject -> Value
dropObjectJson mode obj =
    object
        [ "op" .= opName
        , "target" .= target
        , "mode" .= (if mode == DropCascade then ("cascade" :: T.Text) else "restrict")
        ]
    where
        (opName, target) = case obj of
            DropRelation r kind -> (relKindName kind, nameText (nameRefName r))
            DropIndex i -> ("drop-index" :: T.Text, indexRefName i)
            DropTrigger t -> ("drop-trigger", triggerRefName t)
            DropSequence s -> ("drop-sequence", nameText (nameRefName s))
            DropType t -> ("drop-type", nameText t)
            DropSchema s -> ("drop-schema", nameText s)
        relKindName TableRelation = "drop-table"
        relKindName ViewRelation = "drop-view"
        relKindName MaterializedViewRelation = "drop-materialized-view"

renameJson :: RenameOp -> Value
renameJson (RenameRelation old new _) = renameRelationJson old new
renameJson (RenamePermanentRelation old new _) = renameRelationJson old new
renameJson (RenameColumnOp t old new) =
    object
        [ "op" .= ("rename-column" :: T.Text)
        , "target" .= nameText (nameRefName t)
        , "name" .= identText new
        , "detail" .= identText old
        ]

renameRelationJson :: NameRef -> Name -> Value
renameRelationJson old new =
    object
        [ "op" .= ("rename-relation" :: T.Text)
        , "target" .= nameText (nameRefName old)
        , "name" .= nameText new
        ]

indexRefName :: IndexRef -> T.Text
indexRefName (NamedIndex n) = nameText n
indexRefName (TableIndex t i) = nameText (nameRefName t) <> "." <> identText i

triggerRefName :: TriggerRef -> T.Text
triggerRefName (NamedTrigger n) = nameText n
triggerRefName (TableTrigger t i) = nameText (nameRefName t) <> "." <> identText i

barrierJson :: ScopedBarrier -> Value
barrierJson b =
    object
        [ "scope" .= barrierScopeName (barrierScope b)
        , "reason" .= barrierReasonName (barrierReason b)
        , "detail" .= barrierDetail b
        ]

-- | Wire names for barrier reasons.
barrierReasonName :: BarrierReason -> Text
barrierReasonName reason = case reason of
    SyntaxErrorBarrier -> "SyntaxErrorBarrier"
    UnknownStatementBarrier -> "UnknownStatementBarrier"
    UnknownRelationBarrier -> "UnknownRelationBarrier"
    UnknownDDLBarrier -> "UnknownDDLBarrier"
    UnknownDMLBarrier -> "UnknownDMLBarrier"
    UnknownDropBarrier -> "UnknownDropBarrier"
    UnknownAlterBarrier -> "UnknownAlterBarrier"
    UnknownViewBarrier -> "UnknownViewBarrier"
    UnknownIndexBarrier -> "UnknownIndexBarrier"
    UnknownTriggerBarrier -> "UnknownTriggerBarrier"
    UnknownTruncateBarrier -> "UnknownTruncateBarrier"
    UnknownSessionEffectBarrier -> "UnknownSessionEffectBarrier"
    UnsupportedExpressionBarrier -> "UnsupportedExpressionBarrier"
    UnsupportedConflictClauseBarrier -> "UnsupportedConflictClauseBarrier"
    UnsupportedTriggerBodyBarrier -> "UnsupportedTriggerBodyBarrier"
    UnsupportedTransactionBarrier -> "UnsupportedTransactionBarrier"
    UnresolvedViewSourceBarrier -> "UnresolvedViewSourceBarrier"
    WriteThroughViewBarrier -> "WriteThroughViewBarrier"
    OpaqueTriggerBodyBarrier -> "OpaqueTriggerBodyBarrier"
    UnmodeledRelationSourceBarrier -> "UnmodeledRelationSourceBarrier"

barrierScopeName :: BarrierScope -> T.Text
barrierScopeName GlobalScope = "global"
barrierScopeName (RelationScope r) = "relation:" <> nameText (nameRefName r)
barrierScopeName (SchemaScope n) = "schema:" <> nameText n
barrierScopeName (ObjectScope o) = "object:" <> objectRefName o
barrierScopeName SessionStateScope = "session"
barrierScopeName TransactionStateScope = "transaction"
barrierScopeName (UserVariableScope ident) = "user_var:" <> identText ident

objectRefName :: ObjectRef -> T.Text
objectRefName (ObjectRelation r) = "relation:" <> nameText (nameRefName r)
objectRefName (ObjectIndex i) = "index:" <> indexRefName i
objectRefName (ObjectTrigger t) = "trigger:" <> triggerRefName t
objectRefName (ObjectSequence s) = "sequence:" <> nameText (nameRefName s)
objectRefName (ObjectType t) = "type:" <> nameText t
objectRefName (ObjectSchema s) = "schema:" <> nameText s
objectRefName (ObjectConstraint (ConstraintRef t n)) = "constraint:" <> nameText (nameRefName t) <> "." <> identText n

sessionEffectName :: SessionEffect -> T.Text
sessionEffectName SetVariable {} = "SetVariable"
sessionEffectName (SetSearchPath _ _) = "SetSearchPath"
sessionEffectName (SetNamespace _ _) = "SetNamespace"
sessionEffectName (SetSessionUser _ _) = "SetSessionUser"
sessionEffectName (ResetSessionUser _) = "ResetSessionUser"
sessionEffectName (SetCurrentUser _ _) = "SetCurrentUser"
sessionEffectName (ResetCurrentUser _) = "ResetCurrentUser"
sessionEffectName (SetAutocommit _ _) = "SetAutocommit"
sessionEffectName (SetForeignKeyChecks _ _) = "SetForeignKeyChecks"
sessionEffectName (SetSqliteForeignKeys _) = "SetSqliteForeignKeys"
sessionEffectName (ResetForeignKeyChecks _) = "ResetForeignKeyChecks"
sessionEffectName (ResetVariable _ _) = "ResetVariable"
sessionEffectName (ResetAll _) = "ResetAll"

txEffectName :: TxEffect -> T.Text
txEffectName (TxBegin _) = "TxBegin"
txEffectName (TxCommit _) = "TxCommit"
txEffectName TxRollbackAll = "TxRollbackAll"
txEffectName (TxRollbackTo _) = "TxRollbackTo"
txEffectName (TxSavepoint _) = "TxSavepoint"
txEffectName (TxReleaseSavepoint _) = "TxReleaseSavepoint"
txEffectName (TxSetReadOnly _) = "TxSetReadOnly"

-- | Trigger body and WHEN-clause effects JSON.
triggerEffectsJson :: [CatalogOp] -> Value
triggerEffectsJson ops =
    object ["reads" .= readsJson, "writes" .= writesJson, "other" .= otherJson]
    where
        effects = concatMap triggerBodyEffects ops
        whenReads = concatMap triggerWhenOf ops
        readsJson = map readFactJson (whenReads ++ [r | TriggerRead r <- effects])
        writesJson = [dmlEventJson e | TriggerDml e <- effects]
        otherJson =
            [T.pack (show b) | TriggerBarrier b <- effects]
                ++ [sessionEffectName s | TriggerSession s <- effects]
                ++ ["sequence-write" :: T.Text | TriggerWriteSequence _ <- effects]

triggerWhenOf :: CatalogOp -> [ReadFact]
triggerWhenOf (DefineTriggerOp t) = triggerWhenReads t
triggerWhenOf _ = []

triggerBodyEffects :: CatalogOp -> [TriggerEffect]
triggerBodyEffects (DefineTriggerOp t) = case triggerBody t of
    TriggerInline effs -> effs
    TriggerFunction _ _ -> []
    TriggerOpaque -> []
triggerBodyEffects _ = []

dmlEventJson :: DmlEvent -> Value
dmlEventJson event = case event of
    DmlInsert t cols -> trigWrite "insert" t cols
    DmlUpdate t cols -> trigWrite "update" t cols
    DmlMerge t cols -> trigWrite "merge" t cols
    DmlDelete t _ -> trigWrite "delete" t []
    DmlTruncate t _ -> trigWrite "truncate" t []
    where
        trigWrite kind t cols =
            object
                ["target" .= nameText (nameRefName t), "kind" .= (kind :: T.Text), "columns" .= map identText cols]
