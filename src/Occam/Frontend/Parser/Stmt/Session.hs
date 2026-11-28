-- | Session, privilege, lock, and transaction statement parsers.
module Occam.Frontend.Parser.Stmt.Session (
    parsePragma,
    parseSet,
    parseReset,
    parseLockTables,
    parseUnlockTables,
    parseGrant,
    parseTransaction,
) where

import Control.Monad (guard, join, unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.State.Strict (gets)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Maybe (isJust, isNothing, maybeToList)
import Occam.Frontend.AST.Expr
import Occam.Frontend.AST.Statement
import Occam.Frontend.Lexer hiding (LiteralKind)
import Occam.Frontend.Parser.Core
import Occam.Frontend.Parser.DDL (parseDropBehavior)
import Occam.Frontend.Parser.Expr
import Occam.Frontend.Span (ByteSpan, Located (..), mergeSpan)
import Occam.Parser.Dialect (DialectFeature (..), DialectProfile (..))
import Occam.Parser.Types (SQLDialect (..))

parsePragma :: P (Located PragmaStmt)
parsePragma = withLocated do
    tok <- expectWordCI "PRAGMA"
    requireFeatureP FeaturePragmas (locSpan tok) "PRAGMA"
    name <- parseName
    value <- evalContT do
        onTok TokEq (Just <$> parsePragmaValueAfterEq)
        onTok TokLParen (Just <$> (parsePragmaParenthesized <* expectTok TokRParen))
        pure Nothing
    pure (PragmaStmt name value)

parsePragmaValueAfterEq :: P PragmaValue
parsePragmaValueAfterEq = do
    sqlite <- isDialect SQLite
    if sqlite
        then
            currentWord >>= \case
                Just (tok, raw) -> advanceTok $> PragmaBare (L (locSpan tok) raw)
                Nothing -> PragmaValueExpr <$> parseExpr
        else do
            currentWord >>= traverse_ \(tok, raw) ->
                when (wordIn ["ON", "DELETE", "TRUNCATE"] raw) $
                    unexpectedUnsupported tok "SQLite bare PRAGMA keyword value is not supported in DuckDB"
            PragmaValueExpr <$> parseExpr

parsePragmaParenthesized :: P PragmaValue
parsePragmaParenthesized = do
    sqlite <- isDialect SQLite
    if sqlite
        then
            currentWord >>= \case
                Just (tok, raw) | isBarePragmaValue raw -> advanceTok $> PragmaBare (L (locSpan tok) raw)
                _ -> PragmaValueExpr <$> parseExpr
        else PragmaValueExpr <$> parseExpr

isBarePragmaValue :: ByteString -> Bool
isBarePragmaValue = wordIn ["ON", "OFF", "YES", "NO", "TRUE", "FALSE", "MEMORY", "WAL", "DELETE", "TRUNCATE"]

parseSet :: P (Located SetStmt)
parseSet = withLocated do
    tok <- expectWordCI "SET"
    requireDialectP [PostgreSQL, MySQL, MariaDB, DuckDB, ClickHouse] (locSpan tok) "SET"
    isSessionAuth <- (&&) <$> wordIsCurrent "SESSION" <*> wordIsAt 1 "AUTHORIZATION"
    scope <-
        if isSessionAuth
            then pure SetScopeNone
            else evalContT do
                onWord "SESSION" (pure SetScopeSession)
                onWord "LOCAL" (pure SetScopeLocal)
                onWord "GLOBAL" (pure SetScopeGlobal)
                pure SetScopeNone
    transactionModes <- evalContT do
        onWordTok "TRANSACTION" \txTok -> do
            requireDialectP [PostgreSQL, MySQL, MariaDB] (locSpan txTok) "SET TRANSACTION"
            Just <$> parseSetTransactionModes []
        pure Nothing
    case transactionModes of
        Just modes -> do
            when (null modes) (unexpectedHere "expected a transaction mode after SET TRANSACTION")
            pure (SetStmt scope [] modes)
        Nothing -> SetStmt scope <$> parseSetItems (scope /= SetScopeNone) <*> pure []

parseSetTransactionModes :: [Located ByteString] -> P [Located ByteString]
parseSetTransactionModes acc = do
    mt <- peekTok
    case mt of
        Nothing -> pure (reverse acc)
        Just (L _ TokComma) -> advanceTok *> parseSetTransactionModes acc
        Just _ -> do
            mode <- withLocated parseSetTransactionMode
            parseSetTransactionModes (mode : acc)

parseSetTransactionMode :: P ByteString
parseSetTransactionMode = do
    (_, raw) <-
        currentWord >>= maybe (unexpectedHere "expected a transaction mode") pure
    if asciiEqCI "ISOLATION" raw
        then do
            _ <- advanceTok
            _ <- expectWordCI "LEVEL"
            level <- nextTransactionWord
            case level of
                "SERIALIZABLE" -> pure "ISOLATION LEVEL SERIALIZABLE"
                "REPEATABLE" -> do
                    _ <- expectWordCI "READ"
                    pure "ISOLATION LEVEL REPEATABLE READ"
                "READ" -> do
                    committed <- nextTransactionWord
                    if committed `elem` ["COMMITTED", "UNCOMMITTED"]
                        then pure ("ISOLATION LEVEL READ " <> committed)
                        else unexpectedHere "expected COMMITTED or UNCOMMITTED"
                _ ->
                    unexpectedHere
                        "expected SERIALIZABLE, REPEATABLE READ, READ COMMITTED, or READ UNCOMMITTED"
        else
            if asciiEqCI "READ" raw
                then do
                    _ <- advanceTok
                    mode <- nextTransactionWord
                    if mode `elem` ["WRITE", "ONLY"]
                        then pure ("READ " <> mode)
                        else unexpectedHere "expected WRITE or ONLY"
                else
                    if asciiEqCI "DEFERRABLE" raw
                        then advanceTok $> "DEFERRABLE"
                        else
                            if asciiEqCI "NOT" raw
                                then do
                                    _ <- advanceTok
                                    _ <- expectWordCI "DEFERRABLE"
                                    pure "NOT DEFERRABLE"
                                else
                                    if asciiEqCI "SNAPSHOT" raw
                                        then do
                                            snapTok <- advanceTok
                                            requireDialectP [PostgreSQL] (locSpan snapTok) "SET TRANSACTION SNAPSHOT"
                                            litTok <- advanceTok
                                            case locValue litTok of
                                                TokLiteral kind litRaw
                                                    | kind /= LitNumber -> pure ("SNAPSHOT " <> litRaw)
                                                _ -> unexpectedHere "expected a snapshot string literal after SNAPSHOT"
                                        else unexpectedHere "expected a SET TRANSACTION mode"

nextTransactionWord :: P ByteString
nextTransactionWord =
    currentWord >>= \case
        Just (_, raw) -> advanceTok $> asciiUpperBS raw
        Nothing -> unexpectedHere "expected a transaction mode word"

parseSetItems :: Bool -> P [Located SetItem]
parseSetItems hasExplicitScope = (:) <$> parseSetItem hasExplicitScope <*> go
    where
        go =
            matchTok TokComma >>= \case
                Nothing -> pure []
                Just _ -> do
                    multiple <- allowsMultipleSetItems
                    if multiple
                        then (:) <$> parseSetItem False <*> go
                        else
                            peekTok >>= \case
                                Nothing -> pure []
                                Just tok -> unexpectedUnsupported tok "multiple SET targets are not supported by this dialect"

allowsMultipleSetItems :: P Bool
allowsMultipleSetItems = gets ((`elem` [MySQL, MariaDB, ClickHouse]) . dialectId . psProfile)

parseSetItem :: Bool -> P (Located SetItem)
parseSetItem hasExplicitScope = withLocated $ evalContT do
    onWordTok "NAMES" \tok -> do
        when hasExplicitScope $
            throwParse ParseUnexpectedToken (locSpan tok) "SET NAMES cannot take a GLOBAL/SESSION scope prefix"
        requireDialectP [MySQL, MariaDB] (locSpan tok) "SET NAMES"
        cs <- parseSetValue
        coll <- whenWord "COLLATE" parseSetValue
        pure (SetItem (SetTargetSpecial (L (locSpan tok) "NAMES")) (cs : maybeToList coll) False)
    onWords ["CHARACTER", "CHARSET"] \tok hit -> do
        when hasExplicitScope $
            throwParse
                ParseUnexpectedToken
                (locSpan tok)
                "SET CHARACTER SET cannot take a GLOBAL/SESSION scope prefix"
        requireDialectP [MySQL, MariaDB] (locSpan tok) "SET CHARACTER SET"
        when (asciiEqCI "CHARACTER" hit) (void (expectWordCI "SET"))
        cs <- parseSetValue
        pure (SetItem (SetTargetSpecial (L (locSpan tok) "CHARSET")) [cs] False)
    onWordTok "ROLE" \tok -> do
        requireDialectP [PostgreSQL] (locSpan tok) "SET ROLE"
        roleVal <- parseSetValue
        pure (SetItem (SetTargetSpecial (L (locSpan tok) "ROLE")) [roleVal] False)
    isSessionAuth <- lift ((&&) <$> wordIsCurrent "SESSION" <*> wordIsAt 1 "AUTHORIZATION")
    when isSessionAuth $
        onWordTok "SESSION" \sessTok -> do
            requireDialectP [PostgreSQL] (locSpan sessTok) "SET SESSION AUTHORIZATION"
            authTok <- expectWordCI "AUTHORIZATION"
            val <- parseSetValue
            let targetSpan = mergeSpan (locSpan sessTok) (locSpan authTok)
            pure (SetItem (SetTargetSpecial (L targetSpan "SESSION AUTHORIZATION")) [val] False)
    lift do
        mt <- peekTok
        target <- case mt of
            Just (L sp (TokUserVar raw)) -> do
                when hasExplicitScope $
                    throwParse ParseUnexpectedToken sp "user variable cannot follow GLOBAL/SESSION/LOCAL"
                advanceTok $> SetTargetUserVar (L sp raw)
            Just (L sp (TokSysVar raw)) -> do
                when hasExplicitScope $
                    throwParse ParseUnexpectedToken sp "@@ system variable cannot follow GLOBAL/SESSION/LOCAL"
                advanceTok $> SetTargetSysVar (L sp raw)
            _ -> SetTargetName <$> parseName
        (values, toSyntax) <- evalContT do
            onTok TokEq (liftValues False)
            onWord "TO" (liftValues True)
            lift (unexpectedHere "expected '=' or TO in SET")
        pure (SetItem target values toSyntax)
    where
        liftValues isTo = do
            first <- parseSetValue
            more <- parseSetValueList
            pure (first : more, isTo)

parseSetValue :: P (Located Expr)
parseSetValue =
    peekTok >>= \case
        Just (L sp (TokWord wt))
            | Just uw <- unquotedWord wt
            , isBareSetValue (wordRaw uw) ->
                advanceTok $> L sp (ExprBareKeyword (L sp (wordRaw uw)))
        _ -> parseExpr

parseSetValueList :: P [Located Expr]
parseSetValueList = do
    multiple <- allowsMultipleSetItems
    if multiple
        then pure []
        else do
            startsTarget <- commaStartsSetTarget
            if startsTarget then pure [] else manyPrefix (matchTok TokComma) parseSetValue

{- | True when the upcoming comma introduces a second SET target
(@, name =@ or @, name TO@) rather than another value for the current
target. Multi-target SET is MySQL/MariaDB/ClickHouse only.
-}
commaStartsSetTarget :: P Bool
commaStartsSetTarget = do
    mt <- peekTok
    mName <- wordAt 1
    mAfter <- peekTokAt 2
    pure $ case (mt, mName, mAfter) of
        (Just (L _ TokComma), Just _, Just (L _ TokEq)) -> True
        (Just (L _ TokComma), Just _, Just (L _ (TokWord wt))) ->
            maybe False (asciiEqCI "TO" . wordRaw) (unquotedWord wt)
        _ -> False

isBareSetValue :: ByteString -> Bool
isBareSetValue = wordIn ["ON", "OFF", "YES", "NO", "MEMORY", "ALL", "NONE"]

parseReset :: P (Located ResetStmt)
parseReset = withLocated do
    tok <- expectWordCI "RESET"
    requireDialectP [PostgreSQL, DuckDB] (locSpan tok) "RESET"
    evalContT do
        onWordTok "ALL" \allTok -> do
            requireDialectP [PostgreSQL, DuckDB] (locSpan allTok) "RESET ALL"
            pure ResetAll
        onWordTok "ROLE" \roleTok -> do
            requireDialectP [PostgreSQL] (locSpan roleTok) "RESET ROLE"
            pure ResetRole
        isSessionAuth <- lift ((&&) <$> wordIsCurrent "SESSION" <*> wordIsAt 1 "AUTHORIZATION")
        when isSessionAuth $
            onWordTok "SESSION" \sessTok -> do
                requireDialectP [PostgreSQL] (locSpan sessTok) "RESET SESSION AUTHORIZATION"
                expectWordCI "AUTHORIZATION" $> ResetSessionAuthorization
        lift do
            mt <- peekTok
            target <- case mt of
                Just (L sp (TokUserVar raw)) -> advanceTok $> SetTargetUserVar (L sp raw)
                Just (L sp (TokSysVar raw)) -> advanceTok $> SetTargetSysVar (L sp raw)
                _ -> SetTargetName <$> parseName
            pure (ResetTarget target)

parseLockTables :: P (Located LockTablesStmt)
parseLockTables = withLocated do
    tok <- expectWordCI "LOCK"
    requireDialectP [MySQL, MariaDB] (locSpan tok) "LOCK TABLES"
    _ <-
        matchAnyWordCI ["TABLES", "TABLE"]
            >>= maybe (unexpectedHere "expected TABLES or TABLE after LOCK") pure
    LockTablesStmt <$> parseCommaSeparated parseLockTableItem

parseLockTableItem :: P (Located LockTableItem)
parseLockTableItem = withLocated do
    table <- parseName
    alias <- evalContT do
        onWord "AS" (Just <$> parseAlias False)
        lift do
            mw <- currentWord
            case mw of
                Just (_, w) | not (wordIn ["READ", "WRITE", "LOW_PRIORITY"] w) -> Just <$> parseAlias False
                _ -> pure Nothing
    lockType <- withLocated $ evalContT do
        onWord "READ" do
            local <- isJust <$> matchWordCI "LOCAL"
            pure (if local then "READ LOCAL" else "READ")
        onWord "LOW_PRIORITY" (expectWordCI "WRITE" $> "LOW_PRIORITY WRITE")
        lift (expectWordCI "WRITE" $> "WRITE")
    pure (LockTableItem table alias lockType)

parseUnlockTables :: P (Located Statement)
parseUnlockTables = withLocated do
    tok <- expectWordCI "UNLOCK"
    requireDialectP [MySQL, MariaDB] (locSpan tok) "UNLOCK TABLES"
    _ <-
        matchAnyWordCI ["TABLES", "TABLE"]
            >>= maybe (unexpectedHere "expected TABLES or TABLE after UNLOCK") pure
    pure StatementUnlockTables

parseGrant :: Bool -> P (Located GrantStmt)
parseGrant isRevoke = withLocated do
    tok <- expectWordCI (if isRevoke then "REVOKE" else "GRANT")
    requireDialectP
        [PostgreSQL, MySQL, MariaDB, ClickHouse]
        (locSpan tok)
        (if isRevoke then "REVOKE" else "GRANT")
    privileges <- parseCommaSeparated parseGrantPrivilege
    objectName <- whenWord "ON" do
        _ <- matchAnyWordCI ["TABLE", "FUNCTION", "PROCEDURE", "DATABASE", "SCHEMA", "SEQUENCE"]
        peekToken >>= \case
            Just TokStar -> do
                _ <- advanceTok
                _ <- whenTok TokDot (expectTok TokStar)
                pure Nothing
            _ -> do
                name <- parseName
                _ <- whenTok TokDot (expectTok TokStar)
                pure (Just name)
    _ <- expectWordCI (if isRevoke then "FROM" else "TO")
    grantees <- parseCommaSeparated parseGrantTarget
    _ <- whenWord "WITH" (expectWordsCI ["GRANT", "OPTION"])
    _ <- parseDropBehavior
    pure (GrantStmt isRevoke privileges (join objectName) grantees)
    where
        parseGrantPrivilege = withLocated do
            first <- parseNamePart
            rest <- manyWhile isPrivilegeContWord parseNamePart
            _ <- parseOptParenNonEmptyList parseNamePart
            let wordsBS = map (asciiUpperBS . namePartRaw . locValue) (first : rest)
            pure (BS.intercalate " " wordsBS)
        isPrivilegeContWord =
            maybe False (not . wordIn ["ON", "TO", "FROM"]) <$> currentHeadWord
        parseGrantTarget =
            peekTok >>= \case
                Just (L sp (TokLiteral kind raw))
                    | kind /= LitNumber -> do
                        _ <- advanceTok
                        hostPart <-
                            peekTok >>= \case
                                Just (L sp2 (TokUserVar hostRaw)) -> advanceTok $> Just (L sp2 (NamePart hostRaw False))
                                _ -> pure Nothing
                        let firstPart = L sp (NamePart raw True)
                            parts = firstPart : maybeToList hostPart
                            fullSpan = maybe sp (mergeSpan sp . locSpan) hostPart
                        pure (L fullSpan (Name parts))
                _ -> parseName

parseTransaction :: P (Located TransactionStmt)
parseTransaction = withLocated $ evalContT do
    onWord "BEGIN" do
        sqlite <- isDialect SQLite
        modes <-
            if sqlite
                then do
                    mode <- matchAnyWordCI ["DEFERRED", "IMMEDIATE", "EXCLUSIVE"]
                    _ <- matchWordCI "TRANSACTION"
                    pure (maybeToList (TransactionBeginLock . asciiUpperBS . snd <$> mode))
                else do
                    _ <- matchAnyWordCI ["WORK", "TRANSACTION"]
                    postgres <- isDialect PostgreSQL
                    if postgres then parseTransactionModes PostgreSQL else pure []
        pure (TransactionStmt TransactionBegin Nothing modes Nothing)
    onWord "START" do
        _ <- expectWordCI "TRANSACTION"
        dialect <- gets (dialectId . psProfile)
        modes <- parseTransactionModes dialect
        pure (TransactionStmt TransactionStart Nothing modes Nothing)
    onWord "COMMIT" do
        _ <- matchAnyWordCI ["WORK", "TRANSACTION"]
        TransactionStmt TransactionCommit Nothing [] <$> parseChainClause
    onWord "END" do
        _ <- matchAnyWordCI ["WORK", "TRANSACTION"]
        TransactionStmt TransactionEnd Nothing [] <$> parseChainClause
    onWord "ROLLBACK" do
        _ <- matchAnyWordCI ["WORK", "TRANSACTION"]
        evalContT do
            onWordTok "TO" \toTok -> do
                requireSavepointSupport (locSpan toTok)
                _ <- matchWordCI "SAVEPOINT"
                namePart <- parseNamePart
                pure (TransactionStmt TransactionRollbackTo (Just namePart) [] Nothing)
            lift (TransactionStmt TransactionRollback Nothing [] <$> parseChainClause)
    onWordTok "SAVEPOINT" \tok -> do
        requireSavepointSupport (locSpan tok)
        namePart <- parseNamePart
        pure (TransactionStmt TransactionSavepoint (Just namePart) [] Nothing)
    onWordTok "RELEASE" \tok -> do
        requireSavepointSupport (locSpan tok)
        _ <- matchWordCI "SAVEPOINT"
        namePart <- parseNamePart
        pure (TransactionStmt TransactionReleaseSavepoint (Just namePart) [] Nothing)
    lift (unexpectedHere "expected a transaction command")

requireSavepointSupport :: ByteSpan -> P ()
requireSavepointSupport sp = requireDialectP [PostgreSQL, MySQL, MariaDB, SQLite] sp "SAVEPOINT"

parseTransactionModes :: SQLDialect -> P [TransactionMode]
parseTransactionModes dialect
    | dialect `elem` [PostgreSQL, MySQL, MariaDB] =
        peekTok >>= \case
            Nothing -> pure []
            Just _ -> (:) <$> parseMode <*> manyPrefix separator parseMode
    | otherwise = pure []
    where
        postgres = dialect == PostgreSQL
        separator =
            matchTok TokComma >>= \case
                Just _ -> pure (Just ())
                Nothing
                    | postgres -> do
                        startsMode <- maybe False (wordIn ["ISOLATION", "READ", "NOT", "DEFERRABLE"]) <$> currentHeadWord
                        pure (guard startsMode)
                    | otherwise -> pure Nothing
        parseMode = evalContT do
            onWord "READ" $ do
                (_, access) <-
                    matchAnyWordCI ["ONLY", "WRITE"] >>= maybe (unexpectedHere "expected ONLY or WRITE") pure
                pure (TransactionReadOnly (asciiEqCI "ONLY" access))
            when postgres do
                onWord "ISOLATION" do
                    _ <- expectWordCI "LEVEL"
                    level <- evalContT do
                        onWord "SERIALIZABLE" (pure ["SERIALIZABLE"])
                        onWord "REPEATABLE" (expectWordCI "READ" $> ["REPEATABLE", "READ"])
                        onWord "READ" do
                            (_, kind) <-
                                matchAnyWordCI ["COMMITTED", "UNCOMMITTED"]
                                    >>= maybe
                                        (unexpectedHere "expected COMMITTED or UNCOMMITTED")
                                        pure
                            pure ["READ", kind]
                        lift (unexpectedHere "expected an isolation level")
                    pure (TransactionUnmodeledMode ("ISOLATION LEVEL " <> BS.intercalate " " (map asciiUpperBS level)))
                onWord "NOT" (expectWordCI "DEFERRABLE" $> TransactionUnmodeledMode "NOT DEFERRABLE")
                onWord "DEFERRABLE" (pure (TransactionUnmodeledMode "DEFERRABLE"))
            unless postgres $
                onWord
                    "WITH"
                    (expectWordsCI ["CONSISTENT", "SNAPSHOT"] $> TransactionUnmodeledMode "WITH CONSISTENT SNAPSHOT")
            lift (unexpectedHere "expected a transaction mode")

parseChainClause :: P (Maybe Bool)
parseChainClause = whenWord "AND" (isNothing <$> matchWordCI "NO" <* expectWordCI "CHAIN")
