{-# LANGUAGE ForeignFunctionInterface #-}

module Occam.Parser (
    SQLDialect (..),
    defaultSQLDialect,
    ParserError (..),
    ParsedSQL (..),
    StatementUnit (..),
    StatementFacts (..),
    ParserEdit (..),
    ParserEditCategory (..),
    parseSQL,
    countStatements,
) where

import Control.Exception (finally)
import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    eitherDecodeStrict',
    withObject,
    withText,
    (.:),
    (.:?),
 )
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Foreign (Ptr, alloca, castPtr, nullPtr, peek, poke)
import Foreign.C.Types (CInt (..), CSize (..), CUInt (..))
import Occam.Core (
    DependencyType (..),
    ObjectAction (..),
    ObjectKind (..),
    Span (..),
    Statement (..),
    StatementId (..),
 )

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

data ParserError = ParserError
    { parserErrorCode :: !Int
    , parserErrorMessage :: !Text
    }
    deriving (Eq, Show)

data ParsedSQL = ParsedSQL
    { parsedUnits :: ![StatementUnit]
    }
    deriving (Eq, Show)

data StatementUnit = StatementUnit
    { unitStatement :: !Statement
    , unitFacts :: !(Maybe StatementFacts)
    , unitParseError :: !(Maybe Text)
    }
    deriving (Eq, Show)

data StatementFacts = StatementFacts
    { factActions :: ![ObjectAction]
    , factEdits :: ![ParserEdit]
    }
    deriving (Eq, Show)

data ParserEditCategory
    = EditExpression
    | EditClause
    | EditConstraint
    | EditList
    | EditType
    | EditLiteral
    deriving (Eq, Ord, Show)

instance ToJSON ParserEditCategory where
    toJSON category = toJSON (editCategoryName category)

instance FromJSON ParserEditCategory where
    parseJSON = withText "ParserEditCategory" $ \name ->
        maybe (fail ("unknown parser edit category: " <> T.unpack name)) pure (parseEditCategoryName name)

editCategoryName :: ParserEditCategory -> Text
editCategoryName EditExpression = "expression"
editCategoryName EditClause = "clause"
editCategoryName EditConstraint = "constraint"
editCategoryName EditList = "list"
editCategoryName EditType = "type"
editCategoryName EditLiteral = "literal"

parseEditCategoryName :: Text -> Maybe ParserEditCategory
parseEditCategoryName "expression" = Just EditExpression
parseEditCategoryName "clause" = Just EditClause
parseEditCategoryName "constraint" = Just EditConstraint
parseEditCategoryName "list" = Just EditList
parseEditCategoryName "type" = Just EditType
parseEditCategoryName "literal" = Just EditLiteral
parseEditCategoryName _ = Nothing

data ParserEdit = ParserEdit
    { parserEditStart :: !Int
    , parserEditEnd :: !Int
    , parserEditReplacement :: !ByteString
    , parserEditCategory :: !ParserEditCategory
    , parserEditLabel :: !Text
    }
    deriving (Eq, Show)

data NativeResponse = NativeResponse ![NativeUnit]

data NativeUnit = NativeUnit
    { nativeStart :: !Int
    , nativeEnd :: !Int
    , nativeParseError :: !(Maybe Text)
    , nativeFacts :: !(Maybe NativeFacts)
    }

data NativeFacts = NativeFacts ![ObjectAction] ![ParserEdit]

instance FromJSON NativeResponse where
    parseJSON = withObject "NativeResponse" $ \object -> NativeResponse <$> object .: "units"

instance FromJSON NativeUnit where
    parseJSON = withObject "NativeUnit" $ \object ->
        NativeUnit
            <$> object .: "start"
            <*> object .: "end"
            <*> object .:? "parse_error"
            <*> object .:? "facts"

instance FromJSON NativeFacts where
    parseJSON = withObject "NativeFacts" $ \object -> do
        actions <- object .: "actions"
        edits <- object .: "edits"
        NativeFacts <$> traverse parseAction actions <*> pure edits
        where
            parseAction = withObject "ObjectAction" $ \action -> do
                kindText <- action .: "kind"
                actionText <- action .: "action"
                name <- action .: "name"
                kind <- maybe (fail "sqlparser returned an unknown object kind") pure (parseObjectKind kindText)
                actionType <-
                    maybe (fail "sqlparser returned an unknown dependency action") pure (parseActionType actionText)
                pure (ObjectAction kind actionType name)

instance FromJSON ParserEdit where
    parseJSON = withObject "ParserEdit" $ \object ->
        ParserEdit
            <$> object .: "start"
            <*> object .: "end"
            <*> (TE.encodeUtf8 <$> object .: "replacement")
            <*> object .: "category"
            <*> object .: "label"

foreign import ccall safe "occam_sql_parse"
    c_parseSQL :: Ptr Word8 -> CSize -> CUInt -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "occam_sql_parse_error"
    c_parseError :: CInt -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "occam_sql_count"
    c_countSQL :: Ptr Word8 -> CSize -> CUInt -> Ptr CSize -> IO CInt

foreign import ccall unsafe "occam_sql_free"
    c_freeSQL :: Ptr Word8 -> CSize -> IO ()

parseSQL :: SQLDialect -> ByteString -> IO (Either ParserError ParsedSQL)
parseSQL dialect sql =
    BSU.unsafeUseAsCStringLen sql $ \(input, inputLength) ->
        alloca $ \outputPointer -> alloca $ \outputLength -> do
            poke outputPointer nullPtr
            poke outputLength 0
            status <-
                c_parseSQL
                    (castPtr input)
                    (fromIntegral inputLength)
                    (dialectCode dialect)
                    outputPointer
                    outputLength
            if status /= 0
                then Left <$> nativeError status
                else do
                    pointer <- peek outputPointer
                    length' <- peek outputLength
                    if pointer == nullPtr
                        then pure (Left (ParserError 6 "sqlparser returned a null result buffer"))
                        else do
                            let free = c_freeSQL pointer length'
                                decode = do
                                    bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length')
                                    case eitherDecodeStrict' bytes of
                                        Left message -> pure (Left (ParserError 7 (T.pack message)))
                                        Right (NativeResponse units) ->
                                            pure $ ParsedSQL <$> makeUnits sql units
                            finally decode free
    where
        nativeError status =
            alloca $ \messagePointer -> alloca $ \messageLength -> do
                poke messagePointer nullPtr
                poke messageLength 0
                errorStatus <- c_parseError status messagePointer messageLength
                if errorStatus /= 0
                    then pure (ParserError (fromIntegral status) "sqlparser native parser failed")
                    else do
                        pointer <- peek messagePointer
                        length' <- peek messageLength
                        if pointer == nullPtr
                            then pure (ParserError (fromIntegral status) "sqlparser native parser failed")
                            else do
                                let free = c_freeSQL pointer length'
                                    decode = do
                                        bytes <- BS.packCStringLen (castPtr pointer, fromIntegral length')
                                        pure (ParserError (fromIntegral status) (TE.decodeUtf8Lenient bytes))
                                finally decode free

countStatements :: SQLDialect -> ByteString -> IO (Either ParserError Int)
countStatements dialect sql =
    BSU.unsafeUseAsCStringLen sql $ \(input, inputLength) ->
        alloca $ \countPointer -> do
            poke countPointer 0
            status <- c_countSQL (castPtr input) (fromIntegral inputLength) (dialectCode dialect) countPointer
            if status /= 0
                then Left <$> simpleNativeError status
                else Right . fromIntegral <$> peek countPointer

simpleNativeError :: CInt -> IO ParserError
simpleNativeError status =
    pure $ case status of
        1 -> ParserError 1 "invalid native parser arguments"
        2 -> ParserError 2 "invalid SQL dialect code"
        3 -> ParserError 3 "SQL input is not valid UTF-8"
        4 -> ParserError 4 "sqlparser native parser failed"
        5 -> ParserError 5 "sqlparser native parser panicked"
        _ -> ParserError (fromIntegral status) "unknown sqlparser native error"

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
makeUnits :: ByteString -> [NativeUnit] -> Either ParserError [StatementUnit]
makeUnits source = go 0 1
    where
        sourceLength = BS.length source
        isBoundary offset =
            offset == 0 || offset == sourceLength || BS.index source offset .&. 0xc0 /= 0x80

        go _ _ [] = Right []
        go previousEnd index (native : rest)
            | start < 0 || start >= end || end > sourceLength || start < previousEnd =
                Left (ParserError 8 "sqlparser returned an invalid source byte range")
            | not (isBoundary start && isBoundary end) =
                Left (ParserError 8 "sqlparser returned a source range splitting UTF-8")
            | maybe False (not . all (validEdit start end) . nativeEdits) (nativeFacts native) =
                Left (ParserError 11 "sqlparser returned an invalid edit range")
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
                        && isBoundary (parserEditStart edit)
                        && isBoundary (parserEditEnd edit)

        nativeEdits (NativeFacts _ edits) = edits
        makeFacts (NativeFacts actions edits) = StatementFacts actions edits

parseObjectKind :: Text -> Maybe ObjectKind
parseObjectKind "table" = Just TableObject
parseObjectKind "temporary_table" = Just TemporaryTableObject
parseObjectKind "column" = Just ColumnObject
parseObjectKind "index" = Just IndexObject
parseObjectKind "view" = Just ViewObject
parseObjectKind "trigger" = Just TriggerObject
parseObjectKind "barrier" = Just BarrierObject
parseObjectKind "session_variable" = Just SessionVariableObject
parseObjectKind "transaction" = Just TransactionObject
parseObjectKind _ = Nothing

parseActionType :: Text -> Maybe DependencyType
parseActionType "def" = Just DefDep
parseActionType "use" = Just UseDep
parseActionType "mod" = Just ModDep
parseActionType "drop" = Just DropDep
parseActionType "rename_from" = Just RenameFromDep
parseActionType "rename_to" = Just RenameToDep
parseActionType "barrier" = Just BarrierDep
parseActionType "tx_begin" = Just TxBeginDep
parseActionType "tx_commit" = Just TxCommitDep
parseActionType "tx_rollback" = Just TxRollbackDep
parseActionType "tx_savepoint" = Just TxSavepointDep
parseActionType "tx_rollback_to" = Just TxRollbackToDep
parseActionType "tx_release" = Just TxReleaseDep
parseActionType "cascade" = Just CascadeDep
parseActionType "on_commit_drop" = Just OnCommitDropDep
parseActionType "on_commit_delete_rows" = Just OnCommitDeleteRowsDep
parseActionType _ = Nothing
