-- | Statement dispatch plus DML and session/utility parsers.
module Occam.Frontend.Parser.Stmt (
    parseStatement,
) where

import Control.Monad (guard, join, unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.State.Strict (gets)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (find, traverse_)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust, isNothing, maybeToList)
import Occam.Frontend.AST
import Occam.Frontend.Lexer hiding (LiteralKind)
import Occam.Frontend.Parser.Core
import Occam.Frontend.Parser.DDL
import Occam.Frontend.Parser.Expr
import Occam.Frontend.Parser.Query
import Occam.Frontend.Span (ByteSpan (..), Located (..), mergeSpan, spanEnd, spanStart)
import Occam.Parser.Dialect (DialectFeature (..), DialectProfile (..))
import Occam.Parser.Types (SQLDialect (..))

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

parseRefreshMaterializedView :: P (Located RefreshMaterializedViewStmt)
parseRefreshMaterializedView = withLocated do
    tok <- expectWordCI "REFRESH"
    requireDialectP [PostgreSQL] (locSpan tok) "REFRESH MATERIALIZED VIEW"
    expectWordsCI ["MATERIALIZED", "VIEW"]
    concurrently <- isJust <$> matchWordCI "CONCURRENTLY"
    name <- parseName
    withData <-
        fromMaybe True <$> whenWord "WITH" do
            noData <- isJust <$> matchWordCI "NO"
            _ <- expectWordCI "DATA"
            pure (not noData)
    pure (RefreshMaterializedViewStmt concurrently name withData)

-- ---------------------------------------------------------------------------
-- DML
-- ---------------------------------------------------------------------------

parseInsertFrom :: Bool -> ByteSpan -> Maybe (Located WithClause) -> P (Located InsertStmt)
parseInsertFrom isReplace start mWith = do
    orAction <-
        if isReplace
            then do
                tok <- expectWordCI "REPLACE"
                requireDialectP [SQLite, MySQL, MariaDB] (locSpan tok) "REPLACE"
                when (isJust mWith) $
                    requireDialectP [SQLite] (locSpan tok) "WITH ... REPLACE"
                pure Nothing
            else do
                _ <- expectWordCI "INSERT"
                whenWordTok "OR" \orTok -> do
                    requireDialectP [SQLite] (locSpan orTok) "INSERT OR"
                    action <- evalContT do
                        onWord "ROLLBACK" (pure InsertOrRollback)
                        onWord "ABORT" (pure InsertOrAbort)
                        onWord "FAIL" (pure InsertOrFail)
                        onWord "IGNORE" (pure InsertOrIgnore)
                        onWord "REPLACE" (pure InsertOrReplace)
                        lift (unexpectedHere "expected ABORT, FAIL, IGNORE, REPLACE, or ROLLBACK after INSERT OR")
                    locatedSpanning (locSpan orTok) action
    _ <- matchWordCI "INTO"
    table <- parseName
    partitions <-
        fromMaybe [] <$> whenWordTok "PARTITION" \pTok -> do
            requireDialectP [MySQL, MariaDB] (locSpan pTok) "INSERT ... PARTITION"
            parseParenList parseNamePart
    alias <- parseOptionalAlias False
    opening <- peekToken
    parenQuery <- if opening == Just TokLParen then queryStartsAt 1 else pure False
    columns <- if parenQuery then pure [] else parseOptionalColumnNames
    source <- evalContT do
        onWord "DEFAULT" (expectWordCI "VALUES" $> InsertDefaultValues)
        onWord "VALUES" (InsertValues <$> parseInsertValueRows)
        lift do
            next <- peekToken
            isQuery <- queryStartsAt 0
            if next == Just TokLParen || isQuery
                then InsertQuery <$> parseQuery
                else unexpectedHere "expected VALUES, DEFAULT VALUES, or a query in INSERT"
    valuesAlias <- case source of
        InsertValues _ ->
            whenWordTok "AS" \asTok -> do
                requireDialectP [MySQL] (locSpan asTok) "INSERT ... VALUES ... AS row_alias"
                parseAlias True
        _ -> pure Nothing
    (onConflictClause, onDuplicateClause) <-
        fromMaybe (Nothing, Nothing) <$> whenWordTok "ON" \tok -> evalContT do
            onWord "CONFLICT" do
                requireFeatureP FeatureOnConflict (locSpan tok) "ON CONFLICT"
                clause <- parseOnConflictClause (locSpan tok)
                pure (Just clause, Nothing)
            lift do
                _ <- expectWordCI "DUPLICATE"
                requireFeatureP FeatureOnDuplicateKeyUpdate (locSpan tok) "ON DUPLICATE KEY UPDATE"
                expectWordsCI ["KEY", "UPDATE"]
                assignments <- parseAssignmentList
                pure (Nothing, Just assignments)
    returning <- parseOptionalReturning
    locatedSpanning
        start
        InsertStmt
            { insertWith = mWith
            , insertOrAction = orAction
            , insertTable = table
            , insertPartitions = partitions
            , insertAlias = alias
            , insertColumns = columns
            , insertSource = source
            , insertValuesAlias = valuesAlias
            , insertOnConflict = onConflictClause
            , insertOnDuplicate = onDuplicateClause
            , insertReturning = returning
            , insertReplace = isReplace
            }

-- | MySQL allows @VALUES ROW(...)@ rows; other dialects use @VALUES (...)@.
parseInsertValueRows :: P [[Located Expr]]
parseInsertValueRows = parseCommaSeparated do
    mysql <- isDialect MySQL
    isRow <- if mysql then wordIsCurrent "ROW" else pure False
    if isRow
        then expectWordCI "ROW" *> inParens (parseOptionalCommaList parseExpr)
        else parseParenList parseExpr

parseOnConflictClause :: ByteSpan -> P (Located OnConflictClause)
parseOnConflictClause start = do
    constraint <- whenWordTok "ON" \tok -> do
        requireDialectP [PostgreSQL] (locSpan tok) "ON CONFLICT ON CONSTRAINT"
        expectWordCI "CONSTRAINT" *> parseNamePart
    keys <- parseOptParenNonEmptyList parseIndexKeyPart
    whereExpr <- whenWord "WHERE" parseExpr
    _ <- expectWordCI "DO"
    action <- evalContT do
        onWord "NOTHING" (pure OnConflictDoNothing)
        lift do
            expectWordsCI ["UPDATE", "SET"]
            OnConflictDoUpdate <$> parseAssignmentList <*> whenWord "WHERE" parseExpr
    locatedSpanning start (OnConflictClause constraint keys whereExpr action)

parseOptionalReturning :: P [Located SelectItem]
parseOptionalReturning =
    fromMaybe [] <$> whenWordTok "RETURNING" \tok -> do
        requireFeatureP FeatureReturningClause (locSpan tok) "RETURNING"
        parseCommaSeparated parseSelectItem

parseAssignmentList :: P [Located Assignment]
parseAssignmentList = parseCommaSeparated parseAssignment

parseAssignment :: P (Located Assignment)
parseAssignment = withLocated do
    target <- parseName
    _ <- expectTok TokEq
    Assignment target <$> parseExpr

-- | Parse @UPDATE [ONLY] ... SET ... [FROM ...] [WHERE ...] [ORDER BY ...] [LIMIT ...] [RETURNING ...]@.
parseUpdateFrom :: ByteSpan -> Maybe (Located WithClause) -> P (Located UpdateStmt)
parseUpdateFrom start mWith = do
    _ <- expectWordCI "UPDATE"
    onlyTok <- matchWordCI "ONLY"
    traverse_
        (\tok -> requireDialectP [PostgreSQL] (locSpan tok) "UPDATE ... ONLY")
        onlyTok
    let only = isJust onlyTok
    mysqlLike <- isMySQLFamily
    targets <-
        if mysqlLike
            then parseFromClause
            else FromClause . (: []) <$> withLocated (singleUpdateTarget only)
    _ <- expectWordCI "SET"
    assignments <- parseAssignmentList
    fromClause <- whenWordTok "FROM" \tok -> do
        requireDialectP [PostgreSQL, DuckDB, SQLite] (locSpan tok) "UPDATE ... FROM"
        parseFromClause
    whereExpr <- whenWord "WHERE" parseExpr
    orderBy <-
        fromMaybe [] <$> whenWordTok "ORDER" \tok -> do
            requireDialectP [MySQL, MariaDB, SQLite] (locSpan tok) "UPDATE ... ORDER BY"
            expectWordCI "BY" *> parseCommaSeparated parseOrderItem
    limitClause <- whenWordTok "LIMIT" \tok -> do
        requireDialectP [MySQL, MariaDB, SQLite] (locSpan tok) "UPDATE ... LIMIT"
        parseLimitClause
    returning <- parseOptionalReturning
    locatedSpanning
        start
        UpdateStmt
            { updateWith = mWith
            , updateOnly = only
            , updateTargets = targets
            , updateAssignments = assignments
            , updateFrom = fromClause
            , updateWhere = whereExpr
            , updateOrderBy = orderBy
            , updateLimit = limitClause
            , updateReturning = returning
            }
    where
        singleUpdateTarget isOnly = do
            name <- parseName
            includeDescendants <-
                matchTok TokStar >>= \case
                    Just starTok -> do
                        requireDialectP [PostgreSQL] (locSpan starTok) "UPDATE table * descendant marker"
                        pure True
                    Nothing -> pure False
            alias <- parseOptionalAlias False
            pure
                (TableRef isOnly (L (locSpan name) (TableSourceName name)) includeDescendants alias False Nothing [])

-- | Parse single-table DELETE or either MySQL/MariaDB multi-table form.
parseDeleteFrom :: ByteSpan -> Maybe (Located WithClause) -> P (Located DeleteStmt)
parseDeleteFrom start mWith = do
    _ <- expectWordCI "DELETE"
    mysqlLike <- isMySQLFamily
    (only, includeDescendants, table, alias, targets, fromTables, usingClause) <-
        matchWordCI "FROM" >>= \case
            Just _ -> do
                onlyTok <- matchWordCI "ONLY"
                traverse_ (\tok -> requireDialectP [PostgreSQL] (locSpan tok) "DELETE FROM ONLY") onlyTok
                table <- parseName
                if mysqlLike
                    then do
                        first <- parseDeleteTargetSuffix table
                        rest <- manyPrefix (matchTok TokComma) parseDeleteTarget
                        if deleteTargetHasStar (locValue first) || not (null rest)
                            then do
                                usingClause <- expectWordCI "USING" *> parseFromClause
                                pure (False, False, table, Nothing, first : rest, Nothing, Just usingClause)
                            else do
                                alias <- parseOptionalAlias False
                                usingClause <- whenWord "USING" parseFromClause
                                pure (False, False, table, alias, [first | isJust usingClause], Nothing, usingClause)
                    else do
                        desc <-
                            matchTok TokStar >>= \case
                                Just starTok -> do
                                    requireDialectP [PostgreSQL] (locSpan starTok) "DELETE FROM table * descendant marker"
                                    pure True
                                Nothing -> pure False
                        alias <- parseOptionalAlias False
                        usingClause <- whenWordTok "USING" \tok -> do
                            requireDialectP [PostgreSQL, DuckDB] (locSpan tok) "DELETE ... USING"
                            parseFromClause
                        pure (isJust onlyTok, desc, table, alias, [], Nothing, usingClause)
            Nothing -> do
                sp <- currentSpan
                requireDialectP [MySQL, MariaDB] sp "multi-table DELETE"
                first <- parseDeleteTarget
                rest <- manyPrefix (matchTok TokComma) parseDeleteTarget
                fromClause <- expectWordCI "FROM" *> parseFromClause
                pure
                    (False, False, deleteTargetName (locValue first), Nothing, first : rest, Just fromClause, Nothing)
    whereExpr <- whenWord "WHERE" parseExpr
    orderBy <-
        fromMaybe [] <$> whenWordTok "ORDER" \tok -> do
            requireDialectP [MySQL, MariaDB, SQLite] (locSpan tok) "DELETE ... ORDER BY"
            expectWordCI "BY" *> parseCommaSeparated parseOrderItem
    limitClause <- whenWordTok "LIMIT" \tok -> do
        requireDialectP [MySQL, MariaDB, SQLite] (locSpan tok) "DELETE ... LIMIT"
        parseLimitClause
    returning <- parseOptionalReturning
    when (mysqlLike && not (null returning) && (isJust usingClause || isJust fromTables)) $
        throwParse ParseUnsupported start "multi-table DELETE does not support RETURNING in MariaDB/MySQL"
    locatedSpanning
        start
        DeleteStmt
            { deleteWith = mWith
            , deleteOnly = only
            , deleteIncludeDescendants = includeDescendants
            , deleteTargets = targets
            , deleteTable = table
            , deleteAlias = alias
            , deleteFromTables = fromTables
            , deleteUsing = usingClause
            , deleteWhere = whereExpr
            , deleteOrderBy = orderBy
            , deleteLimit = limitClause
            , deleteReturning = returning
            }
    where
        parseDeleteTarget = parseName >>= parseDeleteTargetSuffix
        parseDeleteTargetSuffix name = do
            star <- whenTok TokDot (expectTok TokStar)
            locatedSpanning (locSpan name) (DeleteTarget name (isJust star))

isMySQLFamily :: P Bool
isMySQLFamily = gets ((`elem` [MySQL, MariaDB]) . dialectId . psProfile)

parseMergeFrom :: ByteSpan -> Maybe (Located WithClause) -> P (Located MergeStmt)
parseMergeFrom start mWith = do
    mergeTok <- expectWordCI "MERGE"
    requireFeatureP FeatureMergeStatement (locSpan mergeTok) "MERGE"
    _ <- expectWordCI "INTO"
    target <- parseName
    targetAlias <- parseOptionalAlias False
    _ <- expectWordCI "USING"
    source <- parseTableRef
    _ <- expectWordCI "ON"
    onExpr <- parseExpr
    clauses <- parseMergeClauses
    returning <- parseOptionalReturning
    locatedSpanning
        start
        MergeStmt
            { mergeWith = mWith
            , mergeTarget = target
            , mergeTargetAlias = targetAlias
            , mergeSource = source
            , mergeOn = onExpr
            , mergeClauses = clauses
            , mergeReturning = returning
            }

parseMergeClauses :: P [Located MergeClause]
parseMergeClauses = (:) <$> parseMergeClause <*> manyWhile (wordIsCurrent "WHEN") parseMergeClause

parseMergeClause :: P (Located MergeClause)
parseMergeClause = withLocated do
    _ <- expectWordCI "WHEN"
    matched <- isNothing <$> matchWordCI "NOT"
    _ <- expectWordCI "MATCHED"
    bySource <- fmap (fromMaybe False) $ whenWord "BY" $ evalContT do
        onWord "SOURCE" (pure True)
        lift (expectWordCI "TARGET" $> False)
    condition <- whenWord "AND" parseExpr
    _ <- expectWordCI "THEN"
    action <- evalContT do
        onWord "UPDATE" (expectWordCI "SET" *> (MergeUpdate <$> parseAssignmentList))
        onWord "INSERT" $ evalContT do
            onWord "DEFAULT" (expectWordCI "VALUES" $> MergeInsertDefaultValues)
            lift do
                columns <- parseOptionalColumnNames
                _ <- expectWordCI "VALUES"
                MergeInsert columns <$> parseParenList parseExpr
        onWord "DELETE" (pure MergeDelete)
        lift (expectWordsCI ["DO", "NOTHING"] $> MergeDoNothing)
    pure (MergeClause matched bySource condition action)

parseUse :: P (Located Name)
parseUse = do
    tok <- expectWordCI "USE"
    requireDialectP [MySQL, MariaDB, DuckDB, ClickHouse] (locSpan tok) "USE"
    parseName

-- ---------------------------------------------------------------------------
-- PRAGMA, SET, transactions
-- ---------------------------------------------------------------------------

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
        Nothing -> SetStmt scope <$> parseSetItems <*> pure []

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

parseSetItems :: P [Located SetItem]
parseSetItems = (:) <$> parseSetItem <*> go
    where
        go =
            matchTok TokComma >>= \case
                Nothing -> pure []
                Just _ -> do
                    multiple <- allowsMultipleSetItems
                    if multiple
                        then (:) <$> parseSetItem <*> go
                        else
                            peekTok >>= \case
                                Nothing -> pure []
                                Just tok -> unexpectedUnsupported tok "multiple SET targets are not supported by this dialect"

allowsMultipleSetItems :: P Bool
allowsMultipleSetItems = gets ((`elem` [MySQL, MariaDB, ClickHouse]) . dialectId . psProfile)

parseSetItem :: P (Located SetItem)
parseSetItem = withLocated $ evalContT do
    onWordTok "NAMES" \tok -> do
        requireDialectP [MySQL, MariaDB] (locSpan tok) "SET NAMES"
        cs <- parseSetValue
        coll <- whenWord "COLLATE" parseSetValue
        pure (SetItem (SetTargetSpecial (L (locSpan tok) "NAMES")) (cs : maybeToList coll) False)
    onWords ["CHARACTER", "CHARSET"] \tok hit -> do
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
            Just (L sp (TokUserVar raw)) -> advanceTok $> SetTargetUserVar (L sp raw)
            Just (L sp (TokSysVar raw)) -> advanceTok $> SetTargetSysVar (L sp raw)
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
    requireDialectP [PostgreSQL, MySQL, MariaDB, DuckDB] (locSpan tok) "RESET"
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

-- ---------------------------------------------------------------------------
-- EXPLAIN, COPY, ANALYZE, VACUUM, ATTACH/DETACH
-- ---------------------------------------------------------------------------

parseExplain :: P (Located ExplainStmt)
parseExplain = withLocated do
    _ <- expectWordCI "EXPLAIN"
    options <- parseExplainOptions []
    _ <- currentHeadWord >>= maybe (unexpectedHere "expected a statement after EXPLAIN") pure
    ExplainStmt (reverse options) <$> parseStatement

parseExplainOptions :: [Located ExplainOption] -> P [Located ExplainOption]
parseExplainOptions acc =
    peekTok >>= \case
        Just (L _ TokLParen) -> do
            opts <- inParens (parseExplainParenOptions [])
            parseExplainOptions (reverse opts ++ acc)
        Just (L _ (TokWord wt))
            | Just uw <- unquotedWord wt
            , asciiEqCI "ANALYZE" (wordRaw uw) || not (isStatementStartWord (wordRaw uw)) -> do
                option <- parseExplainOption
                parseExplainOptions (option : acc)
        _ -> pure acc

parseExplainParenOptions :: [Located ExplainOption] -> P [Located ExplainOption]
parseExplainParenOptions acc =
    peekTok >>= \case
        Just (L _ TokRParen) -> pure (reverse acc)
        Just (L _ TokComma) -> advanceTok *> parseExplainParenOptions acc
        Just (L _ (TokWord _)) -> do
            option <- parseExplainOption
            parseExplainParenOptions (option : acc)
        _ -> pure (reverse acc)

parseExplainOption :: P (Located ExplainOption)
parseExplainOption = do
    nameTok <- advanceTok
    let nameRaw = tokenRaw nameTok
    value <- evalContT do
        onTok TokEq (Just <$> parseExplainOptionValue)
        lift do
            mt <- peekTok
            nextWord <- currentWord
            nextNext <- wordAt 1
            case (mt, nextWord, nextNext) of
                (Just (L _ (TokLiteral _ _)), _, _) -> Just <$> parseExplainOptionValue
                (_, Just (_, raw), Just (_, afterRaw))
                    | not (isStatementStartWord raw)
                    , isStatementStartWord afterRaw -> do
                        valueTok <- advanceTok
                        pure (Just (L (locSpan valueTok) (asciiUpperBS raw)))
                _ -> pure Nothing
    locatedSpanning (locSpan nameTok) (ExplainOption (L (locSpan nameTok) (asciiUpperBS nameRaw)) value)

parseExplainOptionValue :: P (Located ByteString)
parseExplainOptionValue = do
    tok <- advanceTok
    pure (L (locSpan tok) (optionValueRaw tok))

optionValueRaw :: SpannedToken -> ByteString
optionValueRaw tok = case locValue tok of
    TokWord WordToken {wordQuote = Nothing, wordRaw} -> asciiUpperBS wordRaw
    _ -> tokenRaw tok

parseCopy :: P (Located CopyStmt)
parseCopy = withLocated do
    tok <- expectWordCI "COPY"
    requireFeatureP FeatureCopyStatement (locSpan tok) "COPY"
    source <- evalContT do
        onTok TokLParen do
            requireDialectP [PostgreSQL, DuckDB] (locSpan tok) "COPY (query)"
            query <- parseQuery
            _ <- expectTok TokRParen
            pure (CopySourceQuery query)
        lift do
            table <- parseName
            CopySourceTable table <$> parseOptionalColumnNames
    direction <- evalContT do
        onWord "FROM" (pure CopyFrom)
        lift (expectWordCI "TO" $> CopyTo)
    when (isQueryCopy source && direction == CopyFrom) $
        throwParse
            ParseUnsupported
            (locSpan tok)
            "COPY (query) only supports TO"
    target <-
        peekTok >>= \case
            Just (L sp (TokLiteral kind raw)) -> advanceTok $> CopyFile (L sp (Literal kind raw))
            Just (L _ (TokWord wt))
                | Just uw <- unquotedWord wt ->
                    let raw = wordRaw uw
                     in if asciiEqCI "STDIN" raw
                            then advanceTok $> CopyStdin
                            else
                                if asciiEqCI "STDOUT" raw
                                    then advanceTok $> CopyStdout
                                    else
                                        if asciiEqCI "PROGRAM" raw
                                            then do
                                                _ <- advanceTok
                                                litTok <- advanceTok
                                                case locValue litTok of
                                                    TokLiteral kind s -> pure (CopyProgram (L (locSpan litTok) (Literal kind s)))
                                                    _ -> unexpectedHere "expected a program string"
                                            else unexpectedHere "expected a COPY target"
            _ -> unexpectedHere "expected a COPY target"
    CopyStmt source direction target <$> parseCopyOptions
    where
        isQueryCopy = \case
            CopySourceQuery _ -> True
            CopySourceTable _ _ -> False

parseCopyOptions :: P [Located CopyOption]
parseCopyOptions = do
    _ <- matchWordCI "WITH"
    peekTok >>= \case
        Just (L _ TokLParen) -> parseOptionalParenList parseCopyOption
        _ -> parseCopyOptionWords []

parseCopyOptionWords :: [Located CopyOption] -> P [Located CopyOption]
parseCopyOptionWords acc =
    currentWord >>= \case
        Just (tok, raw)
            | not (isStatementStartWord raw) -> do
                _ <- advanceTok
                mValue <- parseOptionalCopyValue
                let nameSpan = locSpan tok
                    optionSpan =
                        ByteSpan
                            (spanStart nameSpan)
                            (maybe (spanEnd nameSpan) (spanEnd . locSpan) mValue)
                parseCopyOptionWords
                    (L optionSpan (CopyOption (L nameSpan (asciiUpperBS raw)) mValue) : acc)
        _ -> pure (reverse acc)

parseCopyOption :: P (Located CopyOption)
parseCopyOption = withLocated do
    nameTok <- advanceTok
    let nameUpper = asciiUpperBS (tokenRaw nameTok)
    mValue <-
        if copyOptionTakesValue nameUpper
            then Just <$> parseExplainOptionValue
            else parseOptionalCopyValue
    pure (CopyOption (L (locSpan nameTok) nameUpper) mValue)

parseOptionalCopyValue :: P (Maybe (Located ByteString))
parseOptionalCopyValue = evalContT do
    lift currentWord >>= traverse_ \(valueTok, valueRaw) ->
        unless (isKnownCopyOption valueRaw) $
            lift (advanceTok $> Just (L (locSpan valueTok) (asciiUpperBS valueRaw))) >>= earlyExit
    lift peekTok >>= traverse_ \case
        tok@(L sp (TokLiteral _ _)) ->
            lift (advanceTok $> Just (L sp (optionValueRaw tok))) >>= earlyExit
        _ -> pure ()
    pure Nothing

copyOptionTakesValue :: ByteString -> Bool
copyOptionTakesValue = wordIn ["FORMAT", "DELIMITER", "NULL", "QUOTE", "ESCAPE", "ENCODING"]

isKnownCopyOption :: ByteString -> Bool
isKnownCopyOption =
    wordIn
        ["HEADER", "CSV", "BINARY", "FREEZE", "OIDS", "DELIMITER", "NULL", "QUOTE", "ESCAPE", "FORMAT"]

parseAnalyze :: P (Located AnalyzeStmt)
parseAnalyze = withLocated do
    tok <- expectWordCI "ANALYZE"
    requireDialectP [PostgreSQL, MySQL, MariaDB, SQLite, DuckDB] (locSpan tok) "ANALYZE"
    _ <- matchWordCI "TABLE"
    verbose <- isJust <$> matchWordCI "VERBOSE"
    AnalyzeStmt verbose <$> parseAnalyzeTables

parseAnalyzeTables :: P [Located AnalyzeTable]
parseAnalyzeTables =
    peekTok >>= \case
        Just (L _ (TokWord _)) -> parseCommaSeparated parseAnalyzeTable
        _ -> pure []

parseAnalyzeTable :: P (Located AnalyzeTable)
parseAnalyzeTable = withLocated (AnalyzeTable <$> parseName <*> parseOptionalColumnNames)

parseVacuum :: P (Located VacuumStmt)
parseVacuum = withLocated do
    tok <- expectWordCI "VACUUM"
    requireDialectP [SQLite, PostgreSQL, DuckDB] (locSpan tok) "VACUUM"
    sqlite <- isDialect SQLite
    if sqlite
        then do
            mIntoFirst <- wordIsCurrent "INTO"
            schema <-
                if mIntoFirst
                    then pure Nothing
                    else
                        peekTok >>= \case
                            Just (L _ (TokWord _)) -> Just <$> parseName
                            _ -> pure Nothing
            into <- whenWord "INTO" parseExpr
            pure (VacuumStmt False False False False schema Nothing [] into)
        else do
            (full, freeze, verbose, analyzeOpt) <- parseVacuumOptions
            (table, cols) <-
                peekTok >>= \case
                    Just (L _ (TokWord _)) -> do
                        mInto <- wordIsCurrent "INTO"
                        if mInto
                            then pure (Nothing, [])
                            else (,) . Just <$> parseName <*> parseOptionalColumnNames
                    _ -> pure (Nothing, [])
            pure (VacuumStmt full freeze verbose analyzeOpt Nothing table cols Nothing)

parseVacuumOptions :: P (Bool, Bool, Bool, Bool)
parseVacuumOptions = go False False False False
    where
        go full freeze verbose analyzeOpt =
            currentHeadWord >>= \case
                Just w
                    | asciiEqCI "FULL" w -> expectWordCI "FULL" *> go True freeze verbose analyzeOpt
                    | asciiEqCI "FREEZE" w -> expectWordCI "FREEZE" *> go full True verbose analyzeOpt
                    | asciiEqCI "VERBOSE" w -> expectWordCI "VERBOSE" *> go full freeze True analyzeOpt
                    | asciiEqCI "ANALYZE" w -> expectWordCI "ANALYZE" *> go full freeze verbose True
                _ -> pure (full, freeze, verbose, analyzeOpt)

-- | Parse SQLite/DuckDB database attachment or ClickHouse object attachment.
parseAttach :: P (Located AttachStmt)
parseAttach = withLocated do
    tok <- expectWordCI "ATTACH"
    requireFeatureP FeatureAttachDetach (locSpan tok) "ATTACH"
    ch <- isDialect ClickHouse
    if ch
        then do
            kind <- parseAttachObjectKind
            ifNotExists <- parseIfNotExists
            name <- parseName
            pure (AttachObject kind name ifNotExists)
        else do
            sqlite <- isDialect SQLite
            _ <- matchWordCI "DATABASE"
            ifNotExists <- parseIfNotExists
            when ifNotExists $
                requireDialectP [DuckDB] (locSpan tok) "ATTACH ... IF NOT EXISTS"
            source <- parseExpr
            alias <-
                whenWord "AS" parseNamePart >>= \case
                    Just a -> pure (Just a)
                    Nothing ->
                        if sqlite
                            then unexpectedHere "ATTACH requires AS alias in SQLite"
                            else pure Nothing
            pure (AttachDatabase source alias ifNotExists)

parseAttachObjectKind :: P (Located ByteString)
parseAttachObjectKind =
    currentWord >>= \case
        Just (tok, raw)
            | wordIn ["TABLE", "DICTIONARY", "DATABASE", "VIEW"] raw ->
                advanceTok $> L (locSpan tok) (asciiUpperBS raw)
        _ -> unexpectedHere "expected TABLE, DICTIONARY, DATABASE, or VIEW"

parseDetach :: P (Located DetachStmt)
parseDetach = withLocated do
    tok <- expectWordCI "DETACH"
    requireFeatureP FeatureAttachDetach (locSpan tok) "DETACH"
    ch <- isDialect ClickHouse
    if ch
        then do
            kind <- parseAttachObjectKind
            ifExists <- parseIfExists
            name <- parseName
            syncFlag <- isJust <$> matchWordCI "SYNC"
            pure (DetachObject kind name ifExists syncFlag)
        else do
            _ <- matchWordCI "DATABASE"
            ifExists <- parseIfExists
            name <- parseName
            pure (DetachDatabase name ifExists)

-- ---------------------------------------------------------------------------
-- PREPARE / EXECUTE / DEALLOCATE / TRUNCATE / PIVOT
-- ---------------------------------------------------------------------------

parsePrepare :: P (Located PrepareStmt)
parsePrepare = withLocated do
    tok <- expectWordCI "PREPARE"
    requireFeatureP FeaturePreparedStatements (locSpan tok) "PREPARE"
    name <- parseName
    mysql <- isDialect MySQL
    mariadb <- isDialect MariaDB
    if mysql || mariadb
        then do
            _ <- expectWordCI "FROM"
            expr <- parseExpr
            when (mysql && not (isMySQLPrepareSource (locValue expr))) $
                throwParse
                    ParseUnsupported
                    (locSpan expr)
                    "MySQL PREPARE ... FROM requires a string literal or user variable"
            pure (PrepareStmt name [] (PrepareExpression expr))
        else do
            params <- parseOptParenList parseTypeName
            unless (null params) $
                requireDialectP [PostgreSQL] (locSpan tok) "typed PREPARE parameter list"
            PrepareStmt name params . PrepareStatement <$> (expectWordCI "AS" *> parseStatement)
    where
        isMySQLPrepareSource = \case
            ExprLiteral (Literal LitNumber _) -> False
            ExprLiteral _ -> True
            ExprCharsetString {} -> True
            ExprParam ParamUser _ -> True
            _ -> False

parseExecute :: P (Located ExecuteStmt)
parseExecute = withLocated do
    tok <- expectWordCI "EXECUTE"
    requireFeatureP FeaturePreparedStatements (locSpan tok) "EXECUTE"
    ExecuteStmt <$> parseName <*> parseOptParenList parseExpr

parseDeallocate :: P (Located DeallocateStmt)
parseDeallocate = withLocated do
    tok <- expectWordCI "DEALLOCATE"
    requireFeatureP FeaturePreparedStatements (locSpan tok) "DEALLOCATE"
    _ <- matchWordCI "PREPARE"
    evalContT do
        onWord "ALL" (pure (DeallocateStmt True Nothing))
        lift (DeallocateStmt False . Just <$> parseName)

parseTruncate :: P (Located TruncateStmt)
parseTruncate = withLocated do
    tok <- expectWordCI "TRUNCATE"
    requireDialectP [PostgreSQL, MySQL, MariaDB, DuckDB, ClickHouse] (locSpan tok) "TRUNCATE"
    _ <- matchWordCI "TABLE"
    onlyTok <- whenWordTok "ONLY" \onlyT -> do
        requireDialectP [PostgreSQL] (locSpan onlyT) "TRUNCATE ... ONLY"
        pure onlyT
    tables <- parseCommaSeparated parseName
    when (length tables > 1) $
        requireDialectP [PostgreSQL] (locSpan tok) "multi-table TRUNCATE"
    identity <- evalContT do
        onWordTok "RESTART" \idTok -> do
            requireDialectP [PostgreSQL] (locSpan idTok) "TRUNCATE ... RESTART IDENTITY"
            expectWordCI "IDENTITY" $> Just True
        onWordTok "CONTINUE" \idTok -> do
            requireDialectP [PostgreSQL] (locSpan idTok) "TRUNCATE ... CONTINUE IDENTITY"
            expectWordCI "IDENTITY" $> Just False
        pure Nothing
    behavior <- parseDropBehavior
    when (isJust behavior) $
        requireDialectP [PostgreSQL] (locSpan tok) "TRUNCATE ... CASCADE/RESTRICT"
    pure (TruncateStmt (isJust onlyTok) tables identity behavior)

parsePivot :: P (Located PivotStmt)
parsePivot = withLocated do
    (headSpan, isUnpivot) <-
        matchAnyWordCI ["PIVOT", "UNPIVOT"]
            >>= maybe
                (unexpectedHere "expected PIVOT or UNPIVOT")
                (\(L sp _, word) -> pure (sp, asciiEqCI "UNPIVOT" word))
    requireFeatureP FeaturePivotUnpivot headSpan (if isUnpivot then "UNPIVOT" else "PIVOT")
    source <- parseTableRef
    _ <- expectWordCI "ON"
    onExprs <- parseCommaSeparated (parseExprPrec precOther)
    if isUnpivot
        then do
            expectWordsCI ["INTO", "NAME"]
            intoName <- parseNamePart
            _ <- expectWordCI "VALUE"
            PivotStmt True source onExprs [] [] [] (Just intoName) . Just <$> parseNamePart
        else do
            inValues <- fromMaybe [] <$> whenWord "IN" (parseParenList parseExpr)
            using <- fromMaybe [] <$> whenWord "USING" (parseCommaSeparated parseSelectItem)
            groupBy <- fromMaybe [] <$> whenWord "GROUP" (expectWordCI "BY" *> parseCommaSeparated parseExpr)
            pure (PivotStmt False source onExprs inValues using groupBy Nothing Nothing)

parseNotify :: P (Located NotifyStmt)
parseNotify = withLocated do
    tok <- expectWordCI "NOTIFY"
    requireDialectP [PostgreSQL] (locSpan tok) "NOTIFY"
    channel <- parseName
    payload <- whenTok TokComma parseExpr
    pure (NotifyStmt channel payload)

parseRenameTable :: P (Located [(Located Name, Located Name)])
parseRenameTable = withLocated do
    tok <- expectWordCI "RENAME"
    _ <- expectWordCI "TABLE"
    requireDialectP [MySQL, MariaDB, ClickHouse] (locSpan tok) "RENAME TABLE"
    parseCommaSeparated ((,) <$> parseName <*> (expectWordCI "TO" *> parseName))

parseOptimize :: P (Located OptimizeStmt)
parseOptimize = withLocated do
    tok <- expectWordCI "OPTIMIZE"
    requireDialectP [ClickHouse, MySQL, MariaDB] (locSpan tok) "OPTIMIZE TABLE"
    _ <- expectWordCI "TABLE"
    table <- parseName
    part <- whenWord "PARTITION" parseExpr
    finalFlag <- isJust <$> matchWordCI "FINAL"
    dedup <- isJust <$> matchWordCI "DEDUPLICATE"
    pure (OptimizeStmt table part finalFlag dedup)

parseShow :: P (Located ShowStmt)
parseShow = withLocated do
    tok <- expectWordCI "SHOW"
    requireDialectP [PostgreSQL, MySQL, MariaDB, DuckDB, ClickHouse] (locSpan tok) "SHOW"
    isCreate <- wordIsCurrent "CREATE"
    (subject, initTarget) <-
        if isCreate
            then do
                kw <- parseNamePart
                obj <- parseNamePart
                target <- Just <$> parseName
                pure ([kw, obj], target)
            else do
                subj <- manyWhile isSubjectWord parseNamePart
                when (null subj) (unexpectedHere "expected subject after SHOW")
                pure (subj, Nothing)
    target <- case initTarget of
        Just t -> pure (Just t)
        Nothing -> evalContT do
            onWords ["FROM", "IN"] \_ _ -> Just <$> parseName
            pure Nothing
    likeExpr <- whenWord "LIKE" parseExpr
    whereExpr <- whenWord "WHERE" parseExpr
    pure (ShowStmt subject target likeExpr whereExpr)
    where
        isSubjectWord =
            maybe False (not . wordIn ["FROM", "IN", "LIKE", "WHERE"]) <$> currentHeadWord
