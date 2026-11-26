-- | Public SQL parsing facade.
module Occam.Parser (
    SQLDialect (..),
    dialectName,
    dialectCode,
    CaseFolding (..),
    applyCaseFolding,
    LexicalProfile (..),
    IdentifierProfile (..),
    DialectFeature (..),
    NamespaceRule (..),
    IndexScopeRule (..),
    TriggerScopeRule (..),
    SemanticsProfile (..),
    DialectProfile (..),
    allDialectProfiles,
    dialectProfile,
    lookupDialectByName,
    dialectHasFeature,
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
    spanOnlyOptions,
    nativeOracleCacheKey,
    nativeDuckDBReplayUnits,
    DuckDBReplayUnit (..),
    factDumpJson,
) where

import Control.Exception (evaluate, finally, mask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Data.Aeson (FromJSON (..), eitherDecodeStrict', encode, withObject, (.:), (.:?))
import Data.Bits (unsafeShiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64, Word8)
import Foreign (Ptr, alloca, castPtr, nullPtr, peek, poke)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt, CSize, CUInt)
import GHC.Clock (getMonotonicTimeNSec)
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
import Occam.Parser.Binary (binaryMagic, decodeBinaryResponse)
import Occam.Parser.Dialect (
    CaseFolding (..),
    DialectFeature (..),
    DialectProfile (..),
    IdentifierProfile (..),
    IndexScopeRule (..),
    LexicalProfile (..),
    NamespaceRule (..),
    SemanticsProfile (..),
    TriggerScopeRule (..),
    allDialectProfiles,
    applyCaseFolding,
    dialectHasFeature,
    dialectProfile,
    lookupDialectByName,
 )
import Occam.Parser.Facts (factDumpJson)
import Occam.Parser.Native (
    NativeResponse (..),
    c_freeSQL,
    c_parseSQL,
    c_parseSQLFlags,
    countStatementsRaw,
    dialectCode,
    nativeError,
    oracleSourceProof,
 )
import Occam.Parser.Types (
    ParseOptions (..),
    ParsedSQL (..),
    ParserError (..),
    ParserErrorKind (..),
    SQLDialect (..),
    ServerConfig (..),
    defaultParseOptions,
    defaultServerConfig,
    dialectName,
 )
import Occam.Parser.Validate (makeUnits, validateColumns)

data DuckDBReplayUnit = DuckDBReplayUnit
    { replayUnitEnd :: !(Maybe Int)
    , replayUnitKind :: !T.Text
    , replayUnitTable :: !(Maybe T.Text)
    }

instance FromJSON DuckDBReplayUnit where
    parseJSON = withObject "DuckDBReplayUnit" \o ->
        DuckDBReplayUnit <$> o .:? "end" <*> o .: "kind" <*> o .:? "table"

-- | Available only for native predicates and for source-independent result shapes.
nativeOracleCacheKey :: SQLDialect -> ByteString -> IO (Maybe ByteString)
nativeOracleCacheKey dialect = oracleSourceProof dialect 0

nativeDuckDBReplayUnits :: ByteString -> IO [DuckDBReplayUnit]
nativeDuckDBReplayUnits sql = do
    bytes <- oracleSourceProof DuckDB 1 sql
    pure $ maybe [] decodeReplayUnits bytes

decodeReplayUnits :: ByteString -> [DuckDBReplayUnit]
decodeReplayUnits bs
    | BS.isPrefixOf "OCCM\x03" bs = decodeBinaryReplayUnits (BS.drop 5 bs)
    | otherwise = fromRight [] (eitherDecodeStrict' bs)

decodeBinaryReplayUnits :: ByteString -> [DuckDBReplayUnit]
decodeBinaryReplayUnits buf
    | BS.length buf < 4 = []
    | otherwise = parseReplayUnits (BS.drop 4 buf) (fromIntegral (readU32LE buf 0))

parseReplayUnits :: ByteString -> Int -> [DuckDBReplayUnit]
parseReplayUnits _ 0 = []
parseReplayUnits cur !remaining
    | BS.length cur < 10 = []
    | otherwise =
        let !endVal = readI64LE cur 0
            !mEnd = if endVal < 0 then Nothing else Just (fromIntegral endVal)
            !kindText = decodeReplayKind (BS.index cur 8)
            (!mTable, !rest) = parseReplayTable (BS.index cur 9) (BS.drop 10 cur)
            !unit = DuckDBReplayUnit mEnd kindText mTable
         in unit : parseReplayUnits rest (remaining - 1)

decodeReplayKind :: Word8 -> T.Text
decodeReplayKind = \case
    1 -> "create"
    2 -> "insert"
    3 -> "query"
    _ -> "other"

parseReplayTable :: Word8 -> ByteString -> (Maybe T.Text, ByteString)
parseReplayTable hasTable afterHdr
    | hasTable /= 1 = (Nothing, afterHdr)
    | BS.length afterHdr < 4 = (Nothing, BS.empty)
    | otherwise =
        let !tLen = fromIntegral (readU32LE afterHdr 0)
            !tblBytes = BS.take tLen (BS.drop 4 afterHdr)
            !afterTbl = BS.drop (4 + tLen) afterHdr
         in (Just (TE.decodeUtf8 tblBytes), afterTbl)

readU32LE :: ByteString -> Int -> Word32
readU32LE s off =
    let b0 = fromIntegral (BS.index s off) :: Word32
        b1 = fromIntegral (BS.index s (off + 1)) :: Word32
        b2 = fromIntegral (BS.index s (off + 2)) :: Word32
        b3 = fromIntegral (BS.index s (off + 3)) :: Word32
     in b0 .|. (b1 `unsafeShiftL` 8) .|. (b2 `unsafeShiftL` 16) .|. (b3 `unsafeShiftL` 24)

readI64LE :: ByteString -> Int -> Int64
readI64LE s off =
    let b0 = fromIntegral (BS.index s off) :: Word64
        b1 = fromIntegral (BS.index s (off + 1)) :: Word64
        b2 = fromIntegral (BS.index s (off + 2)) :: Word64
        b3 = fromIntegral (BS.index s (off + 3)) :: Word64
        b4 = fromIntegral (BS.index s (off + 4)) :: Word64
        b5 = fromIntegral (BS.index s (off + 5)) :: Word64
        b6 = fromIntegral (BS.index s (off + 6)) :: Word64
        b7 = fromIntegral (BS.index s (off + 7)) :: Word64
        w =
            b0
                .|. (b1 `unsafeShiftL` 8)
                .|. (b2 `unsafeShiftL` 16)
                .|. (b3 `unsafeShiftL` 24)
                .|. (b4 `unsafeShiftL` 32)
                .|. (b5 `unsafeShiftL` 40)
                .|. (b6 `unsafeShiftL` 48)
                .|. (b7 `unsafeShiftL` 56)
     in fromIntegral w :: Int64

-- | Untimed parse; delegates to the single timed implementation.
parseSQL :: ParseOptions -> ByteString -> IO (Either ParserError ParsedSQL)
parseSQL = parseSQLTimed Nothing ParseStatementOnly

-- | Parse SQL via native parser with optional metrics recording.
parseSQLTimed ::
    Maybe MetricsStore -> ParseKind -> ParseOptions -> ByteString -> IO (Either ParserError ParsedSQL)
parseSQLTimed store kind options sql = evalContT do
    (input, inputLength) <- ContT $ BSU.unsafeUseAsCStringLen sql
    outputPointer <- ContT alloca
    outputLength <- ContT alloca
    liftIO do
        poke outputPointer nullPtr
        poke outputLength 0
        -- Mask async exceptions across FFI allocation and cleanup registration.
        mask $ \restore -> do
            ffiStart <- clockIf store
            status <- invokeNativeParser options input inputLength outputPointer outputLength
            ffiEnd <- clockIf store
            let ffiMicros = microsBetween ffiStart ffiEnd
            finishNativeParse restore store kind sql ffiMicros ffiEnd status outputPointer outputLength

invokeNativeParser ::
    ParseOptions -> CString -> Int -> Ptr (Ptr Word8) -> Ptr CSize -> IO CInt
invokeNativeParser options input inputLength outputPointer outputLength
    | parseServerConfig options == defaultServerConfig =
        c_parseSQLFlags
            (dialectCode (parseDialect options))
            (parseOptionFlags options)
            (castPtr input)
            (fromIntegral inputLength)
            outputPointer
            outputLength
    | otherwise = evalContT do
        let configBytes = BL.toStrict (encode options)
        (configPointer, configLength) <- ContT $ BSU.unsafeUseAsCStringLen configBytes
        liftIO $
            c_parseSQL
                (castPtr configPointer)
                (fromIntegral configLength)
                (castPtr input)
                (fromIntegral inputLength)
                outputPointer
                outputLength

finishNativeParse ::
    (IO (Either ParserError ParsedSQL) -> IO (Either ParserError ParsedSQL)) ->
    Maybe MetricsStore ->
    ParseKind ->
    ByteString ->
    Int ->
    Word64 ->
    CInt ->
    Ptr (Ptr Word8) ->
    Ptr CSize ->
    IO (Either ParserError ParsedSQL)
finishNativeParse restore store kind sql ffiMicros ffiEnd status outputPointer outputLength
    | status /= 0 = do
        recordParseTiming store kind ffiMicros 0
        Left <$> nativeError status
    | otherwise = do
        pointer <- peek outputPointer
        length' <- peek outputLength
        if pointer == nullPtr
            then do
                recordParseTiming store kind ffiMicros 0
                pure (Left (ParserError ParserNullResult "sqlparser returned a null result buffer"))
            else decodeAllocatedNativeBuffer restore store kind sql ffiMicros ffiEnd pointer length'

decodeAllocatedNativeBuffer ::
    (IO (Either ParserError ParsedSQL) -> IO (Either ParserError ParsedSQL)) ->
    Maybe MetricsStore ->
    ParseKind ->
    ByteString ->
    Int ->
    Word64 ->
    Ptr Word8 ->
    CSize ->
    IO (Either ParserError ParsedSQL)
decodeAllocatedNativeBuffer restore store kind sql ffiMicros ffiEnd pointer length' =
    finally timed (c_freeSQL pointer length')
    where
        -- Zero-copy view over the Rust buffer: every primitive in decodeBinaryResponse
        -- is strictly evaluated/copied before `evaluate (forceParsed parsed)` returns
        -- and `c_freeSQL` runs.
        run = restore $ do
            bytes <- BSU.unsafePackCStringLen (castPtr pointer, fromIntegral length')
            evaluate (forceParsed (decodeNativeResponse sql bytes))
        -- Decode elapsed finalizes even on async abort, before the buffer is freed.
        timed = run `finally` recordDecode
        recordDecode = do
            decodeEnd <- clockIf store
            recordParseTiming store kind ffiMicros (microsBetween ffiEnd decodeEnd)

decodeNativeResponse :: ByteString -> ByteString -> Either ParserError ParsedSQL
decodeNativeResponse sql bytes = decodeRaw >>= buildResponse
    where
        decodeRaw
            | BS.isPrefixOf binaryMagic bytes =
                either (Left . ParserError ParserDecodeFailure) Right $
                    decodeBinaryResponse (BS.drop (BS.length binaryMagic) bytes)
            | otherwise =
                either (Left . ParserError ParserDecodeFailure . T.pack) Right $
                    eitherDecodeStrict' (BS.copy bytes)
        buildResponse response = do
            units <- makeUnits sql (responseUnits response)
            columns <- validateColumns sql units (responseColumns response)
            pure (ParsedSQL (responseInitialSession response) units columns)

clockIf :: Maybe MetricsStore -> IO Word64
clockIf Nothing = pure 0
clockIf (Just _) = getMonotonicTimeNSec

parseOptionFlags :: ParseOptions -> CUInt
parseOptionFlags options =
    (if parseChunkEdits options then 1 else 0)
        .|. (if parseColumnIR options then 2 else 0)
        .|. (if parseColumnBundles options then 4 else 0)
        .|. (if parseSyntaxEdits options then 8 else 0)
        .|. (if parseSyntaxOnly options then 16 else 0)
        .|. (if parseSpanOnly options then 32 else 0)

-- | Parse options for span-and-error-only parsing (no semantic facts or edits).
spanOnlyOptions :: SQLDialect -> ParseOptions
spanOnlyOptions dialect = (defaultParseOptions dialect) {parseSpanOnly = True}

-- | Count SQL statements in the input according to the chosen dialect.
countStatements :: SQLDialect -> ByteString -> IO (Either ParserError Int)
countStatements dialect sql = countStatementsRaw sql (dialectCode dialect)

microsBetween :: Word64 -> Word64 -> Int
microsBetween start end = fromIntegral ((end - start) `div` 1000)

-- | Force the spine of parsed result for accurate timing.
forceParsed :: Either ParserError ParsedSQL -> Either ParserError ParsedSQL
forceParsed (Left err) = err `seq` Left err
forceParsed (Right parsed) =
    length (parsedUnits parsed) `seq` length (parsedColumns parsed) `seq` Right parsed
