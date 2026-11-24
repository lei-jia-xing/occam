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
    nativeOracleCacheKey,
    nativeDuckDBReplayUnits,
    DuckDBReplayUnit (..),
    factDumpJson,
) where

import Control.Exception (evaluate, finally, mask)
import Data.Aeson (FromJSON (..), eitherDecodeStrict', encode, withObject, (.:), (.:?))
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Either (fromRight)
import Data.Text qualified as T
import Data.Word (Word64)
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
    pure $ maybe [] (fromRight [] . eitherDecodeStrict') bytes

-- | Untimed parse; delegates to the single timed implementation.
parseSQL :: ParseOptions -> ByteString -> IO (Either ParserError ParsedSQL)
parseSQL = parseSQLTimed Nothing ParseStatementOnly

-- | Parse SQL via native parser with optional metrics recording.
parseSQLTimed ::
    Maybe MetricsStore -> ParseKind -> ParseOptions -> ByteString -> IO (Either ParserError ParsedSQL)
parseSQLTimed store kind options sql =
    BSU.unsafeUseAsCStringLen sql \(input, inputLength) ->
        alloca \outputPointer -> alloca \outputLength -> do
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
                        else
                            let configBytes = BL.toStrict (encode options)
                             in BSU.unsafeUseAsCStringLen configBytes \(configPointer, configLength) ->
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
                                            parsed =
                                                if BS.isPrefixOf binaryMagic bytes
                                                    then case decodeBinaryResponse (BS.drop (BS.length binaryMagic) bytes) of
                                                        Left message -> Left (ParserError ParserDecodeFailure message)
                                                        Right response -> buildResponse response
                                                    else case eitherDecodeStrict' (BS.copy bytes) of
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
