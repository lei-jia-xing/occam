-- | Public SQL parsing facade.
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
import Data.Word (Word32, Word64)
import Foreign (alloca, castPtr, nullPtr, peek, poke)
import Foreign.C.Types (CUInt)
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
    defaultSQLDialect,
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
    | BS.isPrefixOf "OCCM\x03" bs = decodeBinary (BS.drop 5 bs)
    | otherwise = fromRight [] (eitherDecodeStrict' bs)
    where
        decodeBinary buf
            | BS.length buf < 4 = []
            | otherwise =
                let !count = fromIntegral (readU32LE buf 0) :: Int
                 in parseUnits (BS.drop 4 buf) count
        parseUnits _ 0 = []
        parseUnits cur !remaining
            | BS.length cur < 10 = []
            | otherwise =
                let !endVal = readI64LE cur 0
                    !mEnd = if endVal < 0 then Nothing else Just (fromIntegral endVal)
                    !kindByte = BS.index cur 8
                    !kindText = case kindByte of
                        1 -> "create"
                        2 -> "insert"
                        3 -> "query"
                        _ -> "other"
                    !hasTable = BS.index cur 9
                    (!mTable, !rest) =
                        if hasTable == 1
                            then
                                let curAfterHdr = BS.drop 10 cur
                                 in if BS.length curAfterHdr < 4
                                        then (Nothing, BS.empty)
                                        else
                                            let !tLen = fromIntegral (readU32LE curAfterHdr 0)
                                                !tblBytes = BS.take tLen (BS.drop 4 curAfterHdr)
                                                !afterTbl = BS.drop (4 + tLen) curAfterHdr
                                             in (Just (TE.decodeUtf8 tblBytes), afterTbl)
                            else (Nothing, BS.drop 10 cur)
                    !unit = DuckDBReplayUnit mEnd kindText mTable
                 in unit : parseUnits rest (remaining - 1)

        readU32LE s off =
            let b0 = fromIntegral (BS.index s off) :: Word32
                b1 = fromIntegral (BS.index s (off + 1)) :: Word32
                b2 = fromIntegral (BS.index s (off + 2)) :: Word32
                b3 = fromIntegral (BS.index s (off + 3)) :: Word32
             in b0 .|. (b1 `unsafeShiftL` 8) .|. (b2 `unsafeShiftL` 16) .|. (b3 `unsafeShiftL` 24)

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
            ffiStart <- clockIf
            status <-
                if parseServerConfig options == defaultServerConfig
                    then
                        c_parseSQLFlags
                            (dialectCode (parseDialect options))
                            (parseOptionFlags options)
                            (castPtr input)
                            (fromIntegral inputLength)
                            outputPointer
                            outputLength
                    else do
                        let configBytes = BL.toStrict (encode options)
                        evalContT do
                            (configPointer, configLength) <- ContT $ BSU.unsafeUseAsCStringLen configBytes
                            liftIO $
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
                                    -- Zero-copy view over the Rust buffer: every primitive in
                                    -- decodeBinaryResponse is strictly evaluated/copied before
                                    -- `evaluate (forceParsed parsed)` returns and `free` runs.
                                    bytes <- BSU.unsafePackCStringLen (castPtr pointer, fromIntegral length')
                                    let buildResponse response = do
                                            units <- makeUnits sql (responseUnits response)
                                            columns <- validateColumns sql units (responseColumns response)
                                            pure (ParsedSQL (responseInitialSession response) units columns)
                                        parsed
                                            | BS.isPrefixOf binaryMagic bytes =
                                                case decodeBinaryResponse (BS.drop (BS.length binaryMagic) bytes) of
                                                    Left message -> Left (ParserError ParserDecodeFailure message)
                                                    Right response -> buildResponse response
                                            | otherwise =
                                                case eitherDecodeStrict' (BS.copy bytes) of
                                                    Left message -> Left (ParserError ParserDecodeFailure (T.pack message))
                                                    Right response -> buildResponse response
                                    evaluate (forceParsed parsed)
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
        clockIf = case store of
            Nothing -> pure 0
            Just _ -> getMonotonicTimeNSec

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
