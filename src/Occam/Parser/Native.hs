-- | Raw sqlparser frontend boundary: FFI imports and JSON decoding.
module Occam.Parser.Native (
    NativeResponse (..),
    NativeUnit (..),
    NativeFacts (..),
    c_parseSQL,
    c_parseSQLFlags,
    c_freeSQL,
    nativeError,
    countStatementsRaw,
    dialectCode,
    oracleSourceProof,
) where

import Control.Exception (finally, mask_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Data.Aeson (
    FromJSON (..),
    Value (..),
    withObject,
    (.!=),
    (.:),
    (.:?),
 )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Foreign (Ptr, alloca, castPtr, nullPtr, peek, poke)
import Foreign.C.Types (CInt (..), CSize (..), CUInt (..))
import Occam.Core (SemanticFacts, SessionInit)
import Occam.Facts (ColumnTarget, ParserEdit)
import Occam.Parser.Types (ParserError (..), ParserErrorKind (..), SQLDialect (..))

data NativeResponse = NativeResponse
    { responseInitialSession :: !SessionInit
    , responseUnits :: ![NativeUnit]
    , responseColumns :: ![ColumnTarget]
    }

data NativeUnit = NativeUnit
    { nativeStart :: !Int
    , nativeEnd :: !Int
    , nativeParseError :: !(Maybe Text)
    , nativeCannotProduceResult :: !Bool
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
            <*> objectValue .:? "cannot_produce_result" .!= False
            <*> objectValue .:? "facts"

instance FromJSON NativeFacts where
    parseJSON = withObject "NativeFacts" \objectValue ->
        NativeFacts
            <$> parseJSON (Object objectValue)
            <*> objectValue .: "edits"

foreign import ccall safe "occam_sql_parse"
    c_parseSQL :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall safe "occam_sql_parse_flags"
    c_parseSQLFlags :: CUInt -> CUInt -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "occam_sql_parse_error"
    c_parseError :: CInt -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "occam_sql_count"
    c_countSQL :: Ptr Word8 -> CSize -> CUInt -> Ptr CSize -> IO CInt

foreign import ccall unsafe "occam_sql_free"
    c_freeSQL :: Ptr Word8 -> CSize -> IO ()

foreign import ccall safe "occam_sql_oracle_source"
    c_oracleSource ::
        CUInt -> CUInt -> Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()

oracleSourceProof :: SQLDialect -> CUInt -> ByteString -> IO (Maybe ByteString)
oracleSourceProof dialect mode sql = evalContT do
    (input, size) <- ContT $ BSU.unsafeUseAsCStringLen sql
    output <- ContT alloca
    len <- ContT alloca
    liftIO $ mask_ do
        poke output nullPtr
        poke len 0
        c_oracleSource (dialectCode dialect) mode (castPtr input) (fromIntegral size) output len
        ptr <- peek output
        bytes <- peek len
        if ptr == nullPtr
            then pure Nothing
            else Just <$> (BS.packCStringLen (castPtr ptr, fromIntegral bytes) `finally` c_freeSQL ptr bytes)

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

-- | Decode the native error message for a non-zero parse status.
nativeError :: CInt -> IO ParserError
nativeError status = evalContT do
    messagePointer <- ContT alloca
    messageLength <- ContT alloca
    liftIO do
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

-- | Untimed statement count plus raw pointer decoding.
countStatementsRaw :: ByteString -> CUInt -> IO (Either ParserError Int)
countStatementsRaw sql code = evalContT do
    (input, inputLength) <- ContT $ BSU.unsafeUseAsCStringLen sql
    countPointer <- ContT alloca
    liftIO do
        poke countPointer 0
        status <- c_countSQL (castPtr input) (fromIntegral inputLength) code countPointer
        if status /= 0
            then Left <$> simpleNativeError status
            else Right . fromIntegral <$> peek countPointer

simpleNativeError :: CInt -> IO ParserError
simpleNativeError status =
    let kind = parserErrorKindFromStatus status
     in pure (ParserError kind (parserErrorMessageFor kind))
