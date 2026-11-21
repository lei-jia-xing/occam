module Occam.Parser (
    SQLDialect (..),
    dialectName,
    dialectCode,
    defaultSQLDialect,
    ServerConfig (..),
    defaultServerConfig,
    ParseOptions (..),
    defaultParseOptions,
    ParserError (..),
    ParserErrorKind (..),
    ParsedSQL (..),
    StatementUnit (..),
    StatementFacts (..),
    ParserEdit (..),
    ParserEditCategory (..),
    ColumnSite (..),
    ColumnSiteKind (..),
    ColumnTarget (..),
    parseSQL,
    parseSQLTimed,
    countStatements,
    factDumpJson,
) where

import Control.Exception (evaluate, finally, mask)
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value (..),
    eitherDecodeStrict',
    encode,
    object,
    withObject,
    (.!=),
    (.:),
    (.:?),
    (.=),
 )
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)
import Foreign (Ptr, alloca, castPtr, nullPtr, peek, poke)
import Foreign.C.Types (CInt (..), CSize (..), CUInt (..))
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (
    AlterChange (..),
    AlterTable (..),
    BarrierReason (..),
    BarrierScope (..),
    CatalogOp (..),
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
    NameRef (..),
    ObjectRef (..),
    PartitionLink (..),
    ReadFact (..),
    ReadMode (..),
    RefreshOp (..),
    RelationKind (..),
    RenameOp (..),
    SchemaDef (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    SequenceDef (..),
    SequenceWrite (..),
    SessionEffect (..),
    SessionInit (..),
    Span (..),
    Statement (..),
    StatementId (..),
    TableDef (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEffect (..),
    TriggerRef (..),
    TxEffect (..),
    TypeDef (..),
    ViewDef (..),
    WriteKind (..),
    nameText,
 )
import Occam.Dependency.Enforcement (cascadeWrites)
import Occam.Facts (
    ColumnSite (..),
    ColumnSiteKind (..),
    ColumnTarget (..),
    ParserEdit (..),
    ParserEditCategory (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Metrics (MetricsStore, ParseKind (..), recordParseTiming)

-- Keep dialect selection at the frontend boundary: parsing and every derived
-- fact use the same selected sqlparser dialect.
data SQLDialect
    = GenericSQL
    | PostgreSQL
    | MySQL
    | SQLite
    | DuckDB
    | ClickHouse
    | Oracle
    | Ansi
    | BigQuery
    | Databricks
    | Hive
    | MsSql
    | Redshift
    | Snowflake
    | Spark
    | Teradata
    deriving (Eq, Ord, Show, Enum, Bounded)

defaultSQLDialect :: SQLDialect
defaultSQLDialect = GenericSQL

{- | Server/session configuration that is independent of a particular SQL text.
The graph builder starts from this state and applies session effects as it
scans statements.
-}
data ServerConfig = ServerConfig
    { serverNamespacePrefix :: ![Text]
    {- ^ Each element is one namespace part. A part may be double-quoted to
    preserve case.
    -}
    , serverSearchPath :: ![Text]
    {- ^ Each element is one schema entry. An element may be double-quoted to
    preserve case.
    -}
    , serverCurrentUser :: !(Maybe Text)
    {- ^ An unquoted name follows the dialect's relation folding; a
    double-quoted name preserves case.
    -}
    , serverAutocommit :: !Bool
    , serverForeignKeyChecks :: !(Maybe Bool)
    , serverVariables :: !(Map Text Text)
    , serverLowerCaseTableNames :: !Word8
    , serverTableNamesCaseSensitive :: !(Maybe Bool)
    , serverColumnNamesCaseSensitive :: !(Maybe Bool)
    , serverQuotedIdentifiersCaseSensitive :: !(Maybe Bool)
    }
    deriving (Eq, Show)

defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        { serverNamespacePrefix = []
        , serverSearchPath = []
        , serverCurrentUser = Nothing
        , serverAutocommit = True
        , serverForeignKeyChecks = Nothing
        , serverVariables = Map.empty
        , serverLowerCaseTableNames = 0
        , serverTableNamesCaseSensitive = Nothing
        , serverColumnNamesCaseSensitive = Nothing
        , serverQuotedIdentifiersCaseSensitive = Nothing
        }
data ParseOptions = ParseOptions
    { parseDialect :: !SQLDialect
    , parseServerConfig :: !ServerConfig
    , parseChunkEdits :: !Bool
    , parseColumnIR :: !Bool
    , parseColumnBundles :: !Bool
    , parseSyntaxEdits :: !Bool
    }
    deriving (Eq, Show)

{- | Facts-only defaults: statements, boundaries, and dependency/semantic
facts, but no per-edit syntax IR. Passes that consume 'factEdits' must
request 'parseSyntaxEdits' explicitly (see 'Occam.Reducer' syntax options).
-}
defaultParseOptions :: SQLDialect -> ParseOptions
defaultParseOptions dialect =
    ParseOptions
        { parseDialect = dialect
        , parseServerConfig = defaultServerConfig
        , parseChunkEdits = False
        , parseColumnIR = False
        , parseColumnBundles = False
        , parseSyntaxEdits = False
        }

instance ToJSON ServerConfig where
    toJSON :: ServerConfig -> Value
    toJSON config =
        object
            [ "namespace_prefix" .= serverNamespacePrefix config
            , "search_path" .= serverSearchPath config
            , "current_user" .= serverCurrentUser config
            , "autocommit" .= serverAutocommit config
            , "foreign_key_checks" .= serverForeignKeyChecks config
            , "variables" .= serverVariables config
            , "lower_case_table_names" .= serverLowerCaseTableNames config
            , "table_names_case_sensitive" .= serverTableNamesCaseSensitive config
            , "column_names_case_sensitive" .= serverColumnNamesCaseSensitive config
            , "quoted_identifiers_case_sensitive" .= serverQuotedIdentifiersCaseSensitive config
            ]

instance ToJSON ParseOptions where
    toJSON :: ParseOptions -> Value
    toJSON options =
        object
            [ "dialect" .= dialectName (parseDialect options)
            , "server" .= parseServerConfig options
            , "chunk_edits" .= parseChunkEdits options
            , "column_ir" .= parseColumnIR options
            , "column_bundles" .= parseColumnBundles options
            , "syntax_edits" .= parseSyntaxEdits options
            ]

data ParserErrorKind
    = ParserInvalidArguments
    | ParserInvalidConfig
    | ParserInvalidUtf8
    | ParserNativeFailure
    | ParserNativePanic
    | ParserNullResult
    | ParserDecodeFailure
    | ParserInvalidSourceRange
    | ParserInvalidEditRange
    | ParserUnknownFailure !Int
    deriving (Eq, Show)

data ParserError = ParserError
    { parserErrorKind :: !ParserErrorKind
    , parserErrorMessage :: !Text
    }
    deriving (Eq, Show)

data ParsedSQL = ParsedSQL
    { parsedInitialSession :: !SessionInit
    , parsedUnits :: ![StatementUnit]
    , parsedColumns :: ![ColumnTarget]
    }
    deriving (Eq, Show)

data NativeResponse = NativeResponse
    { responseInitialSession :: !SessionInit
    , responseUnits :: ![NativeUnit]
    , responseColumns :: ![ColumnTarget]
    }

data NativeUnit = NativeUnit
    { nativeStart :: !Int
    , nativeEnd :: !Int
    , nativeParseError :: !(Maybe Text)
    , nativeFacts :: !(Maybe NativeFacts)
    }

data NativeFacts = NativeFacts
    { nativeSemantics :: !SemanticFacts
    , nativeEdits :: ![ParserEdit]
    }

instance FromJSON NativeResponse where
    parseJSON = withObject "NativeResponse" \objectValue ->
        NativeResponse
            <$> objectValue .: "initial_session"
            <*> objectValue .: "units"
            <*> objectValue .:? "columns" .!= []

instance FromJSON NativeUnit where
    parseJSON = withObject "NativeUnit" \objectValue ->
        NativeUnit
            <$> objectValue .: "start"
            <*> objectValue .: "end"
            <*> objectValue .:? "parse_error"
            <*> objectValue .:? "facts"

instance FromJSON NativeFacts where
    parseJSON = withObject "NativeFacts" \objectValue ->
        NativeFacts
            <$> parseJSON (Object objectValue)
            <*> objectValue .: "edits"

foreign import ccall safe "occam_sql_parse"
    c_parseSQL :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "occam_sql_parse_error"
    c_parseError :: CInt -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "occam_sql_count"
    c_countSQL :: Ptr Word8 -> CSize -> CUInt -> Ptr CSize -> IO CInt

foreign import ccall unsafe "occam_sql_free"
    c_freeSQL :: Ptr Word8 -> CSize -> IO ()

-- | Untimed parse; delegates to the single timed implementation.
parseSQL :: ParseOptions -> ByteString -> IO (Either ParserError ParsedSQL)
parseSQL = parseSQLTimed Nothing ParseStatementOnly

{- | The single native-parse implementation. Splits the native FFI call wall
from the JSON-decode plus Haskell-validation wall with honest labels; the
decoded result spine is forced before the decode clock stops, so laziness
cannot report zero for work the caller needs anyway. Clocks and forcing
exist only when a store is present: the unmeasured path pays nothing.
-}
parseSQLTimed ::
    Maybe MetricsStore -> ParseKind -> ParseOptions -> ByteString -> IO (Either ParserError ParsedSQL)
parseSQLTimed store kind options sql =
    BSU.unsafeUseAsCStringLen sql \(input, inputLength) ->
        BSU.unsafeUseAsCStringLen configBytes \(configPointer, configLength) ->
            alloca \outputPointer -> alloca \outputLength -> do
                poke outputPointer nullPtr
                poke outputLength 0
                -- The FFI call through `finally` registration runs masked, so
                -- the natively allocated buffer can never leak to an async
                -- exception arriving between return and handler installation
                -- (safe FFI performs no interruptible C work anyway); only the
                -- decode work inside the installed `finally` runs restored.
                mask $ \restore -> do
                    ffiStart <- clockIf
                    status <-
                        c_parseSQL
                            (castPtr configPointer)
                            (fromIntegral configLength)
                            (castPtr input)
                            (fromIntegral inputLength)
                            outputPointer
                            outputLength
                    ffiEnd <- clockIf
                    let ffiMicros = microsBetween ffiStart ffiEnd
                    if status /= 0
                        then do
                            recordParseTiming store kind ffiMicros 0
                            Left <$> nativeError status
                        else do
                            pointer <- peek outputPointer
                            length' <- peek outputLength
                            if pointer == nullPtr
                                then do
                                    recordParseTiming store kind ffiMicros 0
                                    pure (Left (ParserError ParserNullResult "sqlparser returned a null result buffer"))
                                else do
                                    let free = c_freeSQL pointer length'
                                        run = restore $ do
                                            bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length')
                                            let parsed = case eitherDecodeStrict' bytes of
                                                    Left message -> Left (ParserError ParserDecodeFailure (T.pack message))
                                                    Right response -> do
                                                        units <- makeUnits sql (responseUnits response)
                                                        columns <- validateColumns sql units (responseColumns response)
                                                        pure (ParsedSQL (responseInitialSession response) units columns)
                                            case store of
                                                Nothing -> pure parsed
                                                Just _ -> evaluate (forceParsed parsed)
                                        -- Decode elapsed finalizes even on async abort,
                                        -- before the buffer is freed.
                                        timed =
                                            run
                                                `finally` ( do
                                                                decodeEnd <- clockIf
                                                                recordParseTiming store kind ffiMicros (microsBetween ffiEnd decodeEnd)
                                                          )
                                    finally timed free
    where
        configBytes = BL.toStrict (encode options)
        clockIf = case store of
            Nothing -> pure 0
            Just _ -> getMonotonicTimeNSec
        nativeError status =
            alloca \messagePointer -> alloca \messageLength -> do
                poke messagePointer nullPtr
                poke messageLength 0
                errorStatus <- c_parseError status messagePointer messageLength
                if errorStatus /= 0
                    then pure (ParserError (parserErrorKindFromStatus status) "sqlparser native parser failed")
                    else do
                        pointer <- peek messagePointer
                        length' <- peek messageLength
                        if pointer == nullPtr
                            then pure (ParserError (parserErrorKindFromStatus status) "sqlparser native parser failed")
                            else do
                                let free = c_freeSQL pointer length'
                                    decode = do
                                        bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length')
                                        pure (ParserError (parserErrorKindFromStatus status) (TE.decodeUtf8Lenient bytes))
                                finally decode free

countStatements :: SQLDialect -> ByteString -> IO (Either ParserError Int)
countStatements dialect sql =
    BSU.unsafeUseAsCStringLen sql \(input, inputLength) ->
        alloca \countPointer -> do
            poke countPointer 0
            status <- c_countSQL (castPtr input) (fromIntegral inputLength) (dialectCode dialect) countPointer
            if status /= 0
                then Left <$> simpleNativeError status
                else Right . fromIntegral <$> peek countPointer

simpleNativeError :: CInt -> IO ParserError
simpleNativeError status =
    let kind = parserErrorKindFromStatus status
     in pure (ParserError kind (parserErrorMessageFor kind))

-- Native status codes shared with rust/sqlparser-ffi.
nativeStatusInvalidArguments
    , nativeStatusInvalidConfig
    , nativeStatusInvalidUtf8
    , nativeStatusNativeFailure
    , nativeStatusNativePanic ::
        CInt
nativeStatusInvalidArguments = 1
nativeStatusInvalidConfig = 2
nativeStatusInvalidUtf8 = 3
nativeStatusNativeFailure = 4
nativeStatusNativePanic = 5

parserErrorKindFromStatus :: CInt -> ParserErrorKind
parserErrorKindFromStatus status = case status of
    _ | status == nativeStatusInvalidArguments -> ParserInvalidArguments
    _ | status == nativeStatusInvalidConfig -> ParserInvalidConfig
    _ | status == nativeStatusInvalidUtf8 -> ParserInvalidUtf8
    _ | status == nativeStatusNativeFailure -> ParserNativeFailure
    _ | status == nativeStatusNativePanic -> ParserNativePanic
    _ -> ParserUnknownFailure (fromIntegral status)

parserErrorMessageFor :: ParserErrorKind -> Text
parserErrorMessageFor ParserInvalidArguments = "invalid native parser arguments"
parserErrorMessageFor ParserInvalidConfig = "invalid SQL parser configuration or dialect"
parserErrorMessageFor ParserInvalidUtf8 = "SQL input is not valid UTF-8"
parserErrorMessageFor ParserNativeFailure = "sqlparser native parser failed"
parserErrorMessageFor ParserNativePanic = "sqlparser native parser panicked"
parserErrorMessageFor ParserNullResult = "sqlparser returned a null result buffer"
parserErrorMessageFor ParserDecodeFailure = "sqlparser returned malformed JSON"
parserErrorMessageFor ParserInvalidSourceRange = "sqlparser returned an invalid source byte range"
parserErrorMessageFor ParserInvalidEditRange = "sqlparser returned an invalid edit range"
parserErrorMessageFor (ParserUnknownFailure code) =
    "unknown sqlparser native error " <> T.pack (show code)

dialectName :: SQLDialect -> Text
dialectName GenericSQL = "generic"
dialectName PostgreSQL = "postgresql"
dialectName MySQL = "mysql"
dialectName SQLite = "sqlite"
dialectName DuckDB = "duckdb"
dialectName ClickHouse = "clickhouse"
dialectName Oracle = "oracle"
dialectName Ansi = "ansi"
dialectName BigQuery = "bigquery"
dialectName Databricks = "databricks"
dialectName Hive = "hive"
dialectName MsSql = "mssql"
dialectName Redshift = "redshift"
dialectName Snowflake = "snowflake"
dialectName Spark = "spark"
dialectName Teradata = "teradata"

dialectCode :: SQLDialect -> CUInt
dialectCode GenericSQL = 0
dialectCode PostgreSQL = 1
dialectCode MySQL = 2
dialectCode SQLite = 3
dialectCode DuckDB = 4
dialectCode ClickHouse = 5
dialectCode Oracle = 6
dialectCode Ansi = 7
dialectCode BigQuery = 8
dialectCode Databricks = 9
dialectCode Hive = 10
dialectCode MsSql = 11
dialectCode Redshift = 12
dialectCode Snowflake = 13
dialectCode Spark = 14
dialectCode Teradata = 15

-- | True when the byte offset is zero, end-of-input, or a UTF-8 lead byte.
isUtf8Boundary :: ByteString -> Int -> Bool
isUtf8Boundary source offset =
    offset == 0 || offset == sourceLength || BS.index source offset .&. 0xc0 /= 0x80
    where
        sourceLength = BS.length source

makeUnits :: ByteString -> [NativeUnit] -> Either ParserError [StatementUnit]
makeUnits source = go 0 1
    where
        sourceLength = BS.length source

        go _ _ [] = Right []
        go previousEnd index (native : rest)
            | start < 0 || start >= end || end > sourceLength || start < previousEnd =
                Left (ParserError ParserInvalidSourceRange "sqlparser returned an invalid source byte range")
            | not (isUtf8Boundary source start && isUtf8Boundary source end) =
                Left (ParserError ParserInvalidSourceRange "sqlparser returned a source range splitting UTF-8")
            | maybe False (not . all (validEdit start end) . nativeEdits) (nativeFacts native) =
                Left (ParserError ParserInvalidEditRange "sqlparser returned an invalid edit range")
            | otherwise = do
                let statement = Statement (StatementId index) (Span start end) (BS.take (end - start) (BS.drop start source))
                    facts = fmap makeFacts (nativeFacts native)
                units <- go end (index + 1) rest
                pure (StatementUnit statement facts (nativeParseError native) : units)
            where
                start = nativeStart native
                end = nativeEnd native
                validEdit unitStart unitEnd edit =
                    parserEditStart edit >= unitStart
                        && parserEditStart edit < parserEditEnd edit
                        && parserEditEnd edit <= unitEnd
                        && isUtf8Boundary source (parserEditStart edit)
                        && isUtf8Boundary source (parserEditEnd edit)

        makeFacts native = StatementFacts (nativeSemantics native) (nativeEdits native)

{- | Validate response-level column targets against the units they cite:
statement ids resolve, every site sits inside its unit, and no site splits
UTF-8. Ordering and non-overlap are the frontend's contract, rechecked by
the bundle assembler before any candidate is built.
-}
validateColumns ::
    ByteString -> [StatementUnit] -> [ColumnTarget] -> Either ParserError [ColumnTarget]
validateColumns _ _ [] = Right []
validateColumns source units targets = do
    let unitBounds = IntMap.fromList (zip [1 ..] (map (statementBounds . unitStatement) units))
        badRange = Left (ParserError ParserInvalidEditRange "sqlparser returned an invalid column span")
        checkSite site = do
            unitBound <- maybe badRange Right (IntMap.lookup (siteStmt site) unitBounds)
            if validSite unitBound site
                then Right ()
                else badRange
    mapM_ (mapM_ checkSite . targetSites) targets
    pure targets
    where
        statementBounds statement =
            let Span start end = stmtSpan statement in (start, end)
        validSite (unitStart, unitEnd) site =
            siteStart site >= unitStart
                && siteStart site < siteEnd site
                && siteEnd site <= unitEnd
                && isUtf8Boundary source (siteStart site)
                && isUtf8Boundary source (siteEnd site)

microsBetween :: Word64 -> Word64 -> Int
microsBetween start end = fromIntegral ((end - start) `div` 1000)

{- | Force exactly the result spine the caller traverses anyway. Never a
deepseq of IR for measurement.
-}
forceParsed :: Either ParserError ParsedSQL -> Either ParserError ParsedSQL
forceParsed (Left err) = err `seq` Left err
forceParsed (Right parsed) =
    length (parsedUnits parsed) `seq` length (parsedColumns parsed) `seq` Right parsed

-- ----------------------------------------------------------------------------
-- Fact dump (hidden --dump-facts CLI)
-- ----------------------------------------------------------------------------

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

{- | Reads evaluated when the statement itself runs: direct reads, DML source
reads, and CTAS/view definition sources. Trigger WHEN-clause reads fire
with the trigger, so they are reported under @trigger_effects@ instead.
-}
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
renameJson (RenameRelation old new _) =
    object
        [ "op" .= ("rename-relation" :: T.Text)
        , "target" .= nameText (nameRefName old)
        , "name" .= nameText new
        ]
renameJson (RenameColumnOp t old new) =
    object
        [ "op" .= ("rename-column" :: T.Text)
        , "target" .= nameText (nameRefName t)
        , "name" .= identText new
        , "detail" .= identText old
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

{- | Stable wire names for barrier reasons. Kept explicit so renaming a
constructor cannot silently change the diagnostic JSON.
-}
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

{- | Trigger body and WHEN-clause effects fire when the trigger fires, not at
CREATE time, so they are reported separately from the statement's own reads.
-}
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
