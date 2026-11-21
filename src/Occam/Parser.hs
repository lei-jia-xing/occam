{- | Public SQL parsing facade. Owns the native-call timing/exception
orchestration and re-exports the stable frontend API; the raw FFI boundary,
source validation, and fact diagnostics live in "Occam.Parser.Native",
"Occam.Parser.Validate", and "Occam.Parser.Facts" respectively.
-}
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

countStatements :: SQLDialect -> ByteString -> IO (Either ParserError Int)
countStatements dialect sql = countStatementsRaw sql (dialectCode dialect)

microsBetween :: Word64 -> Word64 -> Int
microsBetween start end = fromIntegral ((end - start) `div` 1000)

{- | Force exactly the result spine the caller traverses anyway. Never a
deepseq of IR for measurement.
-}
forceParsed :: Either ParserError ParsedSQL -> Either ParserError ParsedSQL
forceParsed (Left err) = err `seq` Left err
forceParsed (Right parsed) =
    length (parsedUnits parsed) `seq` length (parsedColumns parsed) `seq` Right parsed
