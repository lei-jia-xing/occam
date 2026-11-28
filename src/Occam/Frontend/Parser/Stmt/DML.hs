-- | DML statement parsers (INSERT/REPLACE, UPDATE, DELETE, MERGE, USE).
module Occam.Frontend.Parser.Stmt.DML (
    parseInsertFrom,
    parseUpdateFrom,
    parseDeleteFrom,
    parseMergeFrom,
    parseUse,
) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.State.Strict (gets)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Occam.Frontend.AST.DML
import Occam.Frontend.AST.Expr
import Occam.Frontend.AST.Query
import Occam.Frontend.Lexer hiding (LiteralKind)
import Occam.Frontend.Parser.Core
import Occam.Frontend.Parser.Expr
import Occam.Frontend.Parser.Query
import Occam.Frontend.Span (ByteSpan, Located (..))
import Occam.Parser.Dialect (DialectFeature (..), DialectProfile (..))
import Occam.Parser.Types (SQLDialect (..))

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
parseOptionalReturning = parseOptionalReturningFor True

parseOptionalReturningFor :: Bool -> P [Located SelectItem]
parseOptionalReturningFor allowMariadb =
    fromMaybe [] <$> whenWordTok "RETURNING" \tok -> do
        requireFeatureP FeatureReturningClause (locSpan tok) "RETURNING"
        unless allowMariadb do
            dialect <- gets (dialectId . psProfile)
            when (dialect == MariaDB) $
                throwParse ParseUnsupported (locSpan tok) "UPDATE ... RETURNING is not supported by mariadb"
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
    returning <- parseOptionalReturningFor False
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
    let isMysqlMultiTable = mysqlLike && (isJust usingClause || isJust fromTables)
    orderBy <-
        fromMaybe [] <$> whenWordTok "ORDER" \tok -> do
            when isMysqlMultiTable $
                throwParse
                    ParseUnsupported
                    (locSpan tok)
                    "multi-table DELETE does not support ORDER BY in MySQL/MariaDB"
            requireDialectP [MySQL, MariaDB, SQLite] (locSpan tok) "DELETE ... ORDER BY"
            expectWordCI "BY" *> parseCommaSeparated parseOrderItem
    limitClause <- whenWordTok "LIMIT" \tok -> do
        when isMysqlMultiTable $
            throwParse
                ParseUnsupported
                (locSpan tok)
                "multi-table DELETE does not support LIMIT in MySQL/MariaDB"
        requireDialectP [MySQL, MariaDB, SQLite] (locSpan tok) "DELETE ... LIMIT"
        parseLimitClause
    returning <- parseOptionalReturning
    when (isMysqlMultiTable && not (null returning)) $
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
