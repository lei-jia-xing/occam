-- | Shared DB-backed syntax probes for corpus and generated cases.
module SyntaxAlignmentChecks.Engine (
    parseCorpusStmts,
    withDuckDBConn,
    duckdbExtractOne,
    sqliteSyntaxCheckWithSchema,
    sqlitePrepareSyntaxCheck,
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket, try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.SQLite3 qualified as Sqlite
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import FrontendCorpusChecks (CorpusCase (..))
import Occam.Frontend.Parser (ParsedScript (..), parseScript)
import Occam.Oracle.DuckDB.FFI (
    DuckDBAPI (..),
    DuckdbConnection,
    DuckdbDatabase,
    closeDatabase,
    getDuckDBAPI,
    openFreshDatabase,
 )
import Occam.Parser.Dialect (DialectProfile)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.HUnit (assertFailure)

-- ---------------------------------------------------------------------------
-- Engine helpers
-- ---------------------------------------------------------------------------

parseCorpusStmts :: DialectProfile -> CorpusCase -> IO Int
parseCorpusStmts profile item =
    case parseScript profile (corpusSql item) of
        Left err ->
            assertFailure (T.unpack (corpusId item) <> ": Occam parseScript failed: " <> show err)
        Right (ParsedScript stmts) -> pure (length stmts)

withDuckDBConn :: ((ByteString -> IO (Either String Int)) -> IO a) -> IO a
withDuckDBConn action = do
    api <- getDuckDBAPI >>= either (assertFailure . ("DuckDB API unavailable: " <>) . show) pure
    bracket (openFreshDatabase api) (closeDatabase api) \database ->
        alloca \connectionPtr -> do
            status <- apiConnect api database connectionPtr
            if status /= 0
                then assertFailure "DuckDB connection failed"
                else do
                    connection <- peek connectionPtr
                    bracket (pure connection) (\_ -> apiDisconnect api connectionPtr) (action . duckdbExtractOnConn api)

duckdbExtractOnConn :: DuckDBAPI -> DuckdbConnection -> ByteString -> IO (Either String Int)
duckdbExtractOnConn api conn sql =
    alloca \extractedPtr -> do
        count <- BSC.useAsCString sql \sqlPtr ->
            apiExtractStatements api conn sqlPtr extractedPtr
        extracted <- peek extractedPtr
        bracket (pure extracted) (\_ -> apiDestroyExtracted api extractedPtr) \_ -> do
            errorPtr <- apiExtractStatementsError api extracted
            if errorPtr == nullPtr
                then pure (Right (fromIntegral count))
                else Left <$> peekCString errorPtr

{-# NOINLINE sharedDuckDBLock #-}
sharedDuckDBLock :: MVar (DuckDBAPI, DuckdbDatabase, DuckdbConnection)
sharedDuckDBLock = unsafePerformIO do
    api <- getDuckDBAPI >>= either (ioError . userError . ("DuckDB unavailable: " <>) . show) pure
    db <- openFreshDatabase api
    conn <- alloca \ptr -> do
        st <- apiConnect api db ptr
        if st /= 0 then ioError (userError "DuckDB connect failed") else peek ptr
    newMVar (api, db, conn)

duckdbExtractOne :: ByteString -> IO (Either String Int)
duckdbExtractOne sql =
    withMVar sharedDuckDBLock \(api, _, conn) ->
        duckdbExtractOnConn api conn sql

{-# NOINLINE sharedSqliteLock #-}
sharedSqliteLock :: MVar Sqlite.Database
sharedSqliteLock = unsafePerformIO do
    db <- Sqlite.open ":memory:"
    Sqlite.exec
        db
        "CREATE TABLE t (a INT PRIMARY KEY, b INT); CREATE TABLE u (a INT PRIMARY KEY, b INT);"
    newMVar db

sqliteSyntaxCheckWithSchema :: ByteString -> IO (Either Text ())
sqliteSyntaxCheckWithSchema sql =
    withMVar sharedSqliteLock \db -> do
        res <-
            try
                ( bracket (Sqlite.prepare db (TE.decodeUtf8 sql)) Sqlite.finalize \_ ->
                    pure ()
                ) ::
                IO (Either Sqlite.SQLError ())
        pure $ case res of
            Right () -> Right ()
            Left err ->
                let details = Sqlite.sqlErrorDetails err
                    lower = T.toLower details
                    isCatalogOnly =
                        any
                            (`T.isInfixOf` lower)
                            [ "no such column"
                            , "no such function"
                            , "no such table"
                            ]
                 in if isCatalogOnly then Right () else Left details

sqlitePrepareSyntaxCheck :: Sqlite.Database -> ByteString -> IO (Either Text ())
sqlitePrepareSyntaxCheck db unitSql = do
    res <-
        try
            ( bracket (Sqlite.prepare db (TE.decodeUtf8 unitSql)) Sqlite.finalize \_ ->
                pure ()
            ) ::
            IO (Either Sqlite.SQLError ())
    pure $ case res of
        Right () -> Right ()
        Left err ->
            let details = Sqlite.sqlErrorDetails err
                lower = T.toLower details
                isSyntax =
                    any
                        (`T.isInfixOf` lower)
                        [ "syntax error"
                        , "incomplete input"
                        , "unrecognized token"
                        , "natural join"
                        , "cannot have both on and using"
                        ]
             in if isSyntax then Left details else Right ()
