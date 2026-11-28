-- | Statement dispatch facade routing top-level SQL statements to domain parsers.
module Occam.Frontend.Parser.Stmt (
    parseStatement,
) where

import Data.ByteString (ByteString)
import Data.Foldable (find)
import Data.Maybe (isJust, isNothing)
import Occam.Frontend.AST.Statement
import Occam.Frontend.Lexer hiding (LiteralKind)
import Occam.Frontend.Parser.Core
import Occam.Frontend.Parser.DDL (parseAlter, parseCreate, parseDrop)
import Occam.Frontend.Parser.Query (parseOptionalWith, parseQueryRest)
import Occam.Frontend.Parser.Stmt.DML (
    parseDeleteFrom,
    parseInsertFrom,
    parseMergeFrom,
    parseUpdateFrom,
    parseUse,
 )
import Occam.Frontend.Parser.Stmt.Session (
    parseGrant,
    parseLockTables,
    parsePragma,
    parseReset,
    parseSet,
    parseTransaction,
    parseUnlockTables,
 )
import Occam.Frontend.Parser.Stmt.Utility (
    parseAnalyze,
    parseAttach,
    parseCopy,
    parseDeallocate,
    parseDetach,
    parseExecute,
    parseExplain,
    parseNotify,
    parseOptimize,
    parsePivot,
    parsePrepare,
    parseRefreshMaterializedView,
    parseRenameTable,
    parseShow,
    parseTruncate,
    parseVacuum,
 )
import Occam.Frontend.Span (Located (..))

-- ---------------------------------------------------------------------------
-- Statement dispatch
-- ---------------------------------------------------------------------------

-- | Parse one statement, including an optional leading WITH clause.
parseStatement :: P (Located Statement)
parseStatement = do
    start <- currentSpan
    mWith <- parseOptionalWith
    mt <- peekTok
    case mt of
        Nothing -> unexpectedHere "expected a statement"
        Just (L sp tok)
            | Just w <- headWordOf tok
            , wordIn ["SELECT", "VALUES", "TABLE", "FROM"] w ->
                rewrap StatementQuery <$> parseQueryRest start mWith
            | Just w <- headWordOf tok
            , asciiEqCI "INSERT" w ->
                rewrap StatementInsert <$> parseInsertFrom False start mWith
            | Just w <- headWordOf tok
            , asciiEqCI "REPLACE" w ->
                rewrap StatementInsert <$> parseInsertFrom True start mWith
            | Just w <- headWordOf tok
            , asciiEqCI "UPDATE" w ->
                rewrap StatementUpdate <$> parseUpdateFrom start mWith
            | Just w <- headWordOf tok
            , asciiEqCI "DELETE" w ->
                rewrap StatementDelete <$> parseDeleteFrom start mWith
            | Just w <- headWordOf tok
            , asciiEqCI "MERGE" w ->
                rewrap StatementMerge <$> parseMergeFrom start mWith
            | TokLParen <- tok
            , isNothing mWith ->
                rewrap StatementQuery <$> parseQueryRest start Nothing
            | isJust mWith ->
                throwParse
                    ParseUnsupported
                    sp
                    "WITH is only supported before a query, INSERT, REPLACE, UPDATE, DELETE, or MERGE statement"
            | otherwise -> parseNoWithStatement

parseNoWithStatement :: P (Located Statement)
parseNoWithStatement = do
    mw <- currentHeadWord
    case mw of
        Nothing -> unexpectedHere "expected a statement"
        Just w -> case find (\(kw, _) -> asciiEqCI kw w) statementDispatch of
            Just (_, action) -> action
            Nothing -> do
                mt <- peekTok
                case mt of
                    Just tok -> unexpectedUnsupported tok ("unsupported statement " <> decodeBS w)
                    Nothing -> unexpectedHere "expected a statement"
    where
        statementDispatch :: [(ByteString, P (Located Statement))]
        statementDispatch =
            [ ("CREATE", rewrap StatementCreate <$> parseCreate)
            , ("DROP", rewrap StatementDrop <$> parseDrop)
            , ("ALTER", rewrap StatementAlter <$> parseAlter)
            , ("PRAGMA", rewrap StatementPragma <$> parsePragma)
            , ("SET", rewrap StatementSet <$> parseSet)
            , ("RESET", rewrap StatementReset <$> parseReset)
            , ("LOCK", rewrap StatementLockTables <$> parseLockTables)
            , ("UNLOCK", parseUnlockTables)
            , ("GRANT", rewrap StatementGrant <$> parseGrant False)
            , ("REVOKE", rewrap StatementGrant <$> parseGrant True)
            , ("USE", rewrap StatementUse <$> parseUse)
            , ("EXPLAIN", rewrap StatementExplain <$> parseExplain)
            , ("COPY", rewrap StatementCopy <$> parseCopy)
            , ("ANALYZE", rewrap StatementAnalyze <$> parseAnalyze)
            , ("VACUUM", rewrap StatementVacuum <$> parseVacuum)
            , ("ATTACH", rewrap StatementAttach <$> parseAttach)
            , ("DETACH", rewrap StatementDetach <$> parseDetach)
            , ("PREPARE", rewrap StatementPrepare <$> parsePrepare)
            , ("EXECUTE", rewrap StatementExecute <$> parseExecute)
            , ("DEALLOCATE", rewrap StatementDeallocate <$> parseDeallocate)
            , ("TRUNCATE", rewrap StatementTruncate <$> parseTruncate)
            , ("PIVOT", rewrap StatementPivot <$> parsePivot)
            , ("UNPIVOT", rewrap StatementPivot <$> parsePivot)
            , ("NOTIFY", rewrap StatementNotify <$> parseNotify)
            , ("RENAME", rewrap StatementRenameTable <$> parseRenameTable)
            , ("OPTIMIZE", rewrap StatementOptimize <$> parseOptimize)
            , ("SHOW", rewrap StatementShow <$> parseShow)
            , ("REFRESH", rewrap StatementRefreshMaterializedView <$> parseRefreshMaterializedView)
            , ("BEGIN", tx)
            , ("START", tx)
            , ("COMMIT", tx)
            , ("ROLLBACK", tx)
            , ("SAVEPOINT", tx)
            , ("RELEASE", tx)
            , ("END", tx)
            ]
        tx = rewrap StatementTransaction <$> parseTransaction
