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
    factDumpJson,
) where

import Control.Exception (evaluate, finally, mask)
import Data.Aeson (eitherDecodeStrict', encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import Foreign (alloca, castPtr, nullPtr, peek, poke)
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
    countStatementsRaw,
    dialectCode,
    nativeError,
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

-- | Untimed parse; delegates to the single timed implementation.
parseSQL :: ParseOptions -> ByteString -> IO (Either ParserError ParsedSQL)
parseSQL = parseSQLTimed Nothing ParseStatementOnly

-- | Parse SQL via native parser with optional metrics recording.
parseSQLTimed ::
    Maybe MetricsStore -> ParseKind -> ParseOptions -> ByteString -> IO (Either ParserError ParsedSQL)
parseSQLTimed store kind options sql =
    BSU.unsafeUseAsCStringLen sql \(input, inputLength) ->
        BSU.unsafeUseAsCStringLen configBytes \(configPointer, configLength) ->
            alloca \outputPointer -> alloca \outputLength -> do
                poke outputPointer nullPtr
                poke outputLength 0
                -- Mask async exceptions across FFI allocation and cleanup registration.
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
                                            let buildResponse response = do
                                                    units <- makeUnits sql (responseUnits response)
                                                    columns <- validateColumns sql units (responseColumns response)
                                                    pure (ParsedSQL (responseInitialSession response) units columns)
                                                parsed =
                                                    if BS.isPrefixOf binaryMagic bytes
                                                        then case decodeBinaryResponse (BS.drop (BS.length binaryMagic) bytes) of
                                                            Left message -> Left (ParserError ParserDecodeFailure message)
                                                            Right response -> buildResponse response
                                                        else case eitherDecodeStrict' bytes of
                                                            Left message -> Left (ParserError ParserDecodeFailure (T.pack message))
                                                            Right response -> buildResponse response
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
        configBytes = encodeOptionsFast options
        clockIf = case store of
            Nothing -> pure 0
            Just _ -> getMonotonicTimeNSec

defaultServerConfigJson :: ByteString
defaultServerConfigJson = BL.toStrict (encode defaultServerConfig)

boolJsonBytes :: Bool -> ByteString
boolJsonBytes True = "true"
boolJsonBytes False = "false"

encodeOptionsFast :: ParseOptions -> ByteString
encodeOptionsFast options
    | parseServerConfig options == defaultServerConfig =
        BS.concat
            [ "{\"dialect\":\""
            , TE.encodeUtf8 (dialectName (parseDialect options))
            , "\",\"server\":"
            , defaultServerConfigJson
            , ",\"chunk_edits\":"
            , boolJsonBytes (parseChunkEdits options)
            , ",\"column_ir\":"
            , boolJsonBytes (parseColumnIR options)
            , ",\"column_bundles\":"
            , boolJsonBytes (parseColumnBundles options)
            , ",\"syntax_edits\":"
            , boolJsonBytes (parseSyntaxEdits options)
            , "}"
            ]
    | otherwise = BL.toStrict (encode options)

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
