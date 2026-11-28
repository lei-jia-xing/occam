-- | DROP and ALTER statement parsers.
module Occam.Frontend.Parser.DDL.AlterDrop (
    parseDrop,
    parseAlter,
    parseIfExists,
    parseDropBehavior,
) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Occam.Frontend.AST.DDL
import Occam.Frontend.AST.DML (Assignment (..))
import Occam.Frontend.AST.Expr
import Occam.Frontend.Lexer hiding (LiteralKind)
import Occam.Frontend.Parser.Core
import Occam.Frontend.Parser.DDL.CreateTable (
    ColumnDefOpts (..),
    parseColumnDef,
    parseColumnDefWith,
    parseIfNotExists,
    parsePartitionBoundSpec,
    parseTableConstraint,
    parseTableOption,
    parseTableOptionList,
    validatePgRelationName,
 )
import Occam.Frontend.Parser.Expr
import Occam.Frontend.Parser.Query (parseQuery)
import Occam.Frontend.Span (Located (..), mergeSpan)
import Occam.Parser.Dialect (DialectFeature (..))
import Occam.Parser.Types (SQLDialect (..))

-- | Consume optional @IF EXISTS@ and report whether it occurred.
parseIfExists :: P Bool
parseIfExists = isJust <$> whenWord "IF" (expectWordCI "EXISTS")

-- ---------------------------------------------------------------------------
-- DROP
-- ---------------------------------------------------------------------------

-- | Parse a DROP statement with IF EXISTS, name list, and behavior.
parseDrop :: P (Located DropStmt)
parseDrop = withLocated do
    _ <- expectWordCI "DROP"
    tempTokOpt <- matchWordCI "TEMPORARY"
    traverse_
        (\tok -> requireDialectP [MySQL, MariaDB, ClickHouse] (locSpan tok) "DROP TEMPORARY")
        tempTokOpt
    (objTok, objRaw) <-
        currentWord >>= maybe (unexpectedHere "expected an object type after DROP") pure
    let upper = asciiUpperBS objRaw
    (dropObj, objSpan) <-
        if upper == "MATERIALIZED"
            then do
                materializedTok <- expectWordCI "MATERIALIZED"
                requireFeatureP FeatureDropMaterializedViews (locSpan materializedTok) "DROP MATERIALIZED VIEW"
                viewTok <- expectWordCI "VIEW"
                pure (DropMaterializedView, mergeSpan (locSpan materializedTok) (locSpan viewTok))
            else (,locSpan objTok) <$> parseDropObjectWord upper objTok
    traverse_
        ( \tok -> case dropObj of
            DropTable -> requireDialectP [MySQL, MariaDB, ClickHouse] (locSpan tok) "DROP TEMPORARY TABLE"
            DropView -> requireDialectP [ClickHouse] (locSpan tok) "DROP TEMPORARY VIEW"
            DropSequence -> requireDialectP [MariaDB] (locSpan tok) "DROP TEMPORARY SEQUENCE"
            _ -> throwParse ParseUnsupported (locSpan tok) "TEMPORARY is only valid for DROP TABLE/VIEW/SEQUENCE"
        )
        tempTokOpt
    ifExists <- parseIfExists
    names <- parseCommaSeparated parseName
    onTable <- whenWordTok "ON" \tok -> do
        case dropObj of
            DropIndex ->
                requireDialectP [MySQL, MariaDB, ClickHouse] (locSpan tok) "DROP INDEX ... ON table"
            DropTrigger ->
                requireDialectP [PostgreSQL] (locSpan tok) "DROP TRIGGER ... ON table"
            DropPolicy ->
                requireDialectP [PostgreSQL] (locSpan tok) "DROP POLICY ... ON table"
            DropRule ->
                requireDialectP [PostgreSQL] (locSpan tok) "DROP RULE ... ON table"
            _ ->
                unexpectedUnsupported tok "DROP ... ON table is not valid for this object type"
        parseName
    mysqlLike <- (||) <$> isDialect MySQL <*> isDialect MariaDB
    ch <- isDialect ClickHouse
    let isIndexDrop = case dropObj of
            DropIndex -> True
            _ -> False
        requiresOn = case dropObj of
            DropTrigger -> True
            DropPolicy -> True
            DropRule -> True
            _ -> False
    when
        ((mysqlLike || ch) && isIndexDrop && isNothing onTable)
        (throwParse ParseUnsupported objSpan "DROP INDEX requires ON table in MySQL/MariaDB/ClickHouse")
    postgres <- isDialect PostgreSQL
    when
        (postgres && requiresOn && isNothing onTable)
        (throwParse ParseUnsupported objSpan "DROP TRIGGER/POLICY/RULE requires ON table in PostgreSQL")
    behavior <- parseDropBehavior
    sqlite <- isDialect SQLite
    when (sqlite && isJust behavior) $
        throwParse ParseUnsupported objSpan "SQLite does not support CASCADE or RESTRICT in DROP"
    syncFlag <-
        isJust <$> whenWordTok "SYNC" \tok ->
            requireDialectP [ClickHouse] (locSpan tok) "DROP ... SYNC"
    pure (DropStmt (isJust tempTokOpt) (L objSpan dropObj) ifExists names onTable behavior syncFlag)

parseDropObjectWord :: ByteString -> SpannedToken -> P DropObject
parseDropObjectWord upper objTok =
    let sp = locSpan objTok
        step p = advanceTok *> p
     in case upper of
            "TABLE" -> step (pure DropTable)
            "INDEX" ->
                step
                    ( requireDialectP [PostgreSQL, MySQL, MariaDB, SQLite, DuckDB, ClickHouse] sp "DROP INDEX"
                        $> DropIndex
                    )
            "VIEW" -> step (pure DropView)
            "SEQUENCE" -> step (requireFeatureP FeatureSequenceDDL sp "DROP SEQUENCE" $> DropSequence)
            "SCHEMA" ->
                step
                    (requireDialectP [PostgreSQL, MySQL, MariaDB, DuckDB] sp "DROP SCHEMA" $> DropSchema)
            "DATABASE" ->
                step
                    (requireDialectP [PostgreSQL, MySQL, MariaDB, ClickHouse] sp "DROP DATABASE" $> DropDatabase)
            "TRIGGER" ->
                step (requireDialectP [PostgreSQL, MySQL, MariaDB, SQLite] sp "DROP TRIGGER" $> DropTrigger)
            "FUNCTION" ->
                step
                    (requireDialectP [PostgreSQL, MySQL, MariaDB, DuckDB, ClickHouse] sp "DROP FUNCTION" $> DropFunction)
            "PROCEDURE" ->
                step (requireDialectP [PostgreSQL, MySQL, MariaDB] sp "DROP PROCEDURE" $> DropProcedure)
            "AGGREGATE" ->
                step (requireDialectP [PostgreSQL] sp "DROP AGGREGATE" $> DropAggregate)
            "STATISTICS" -> step (requireFeatureP FeatureCreateStatistics sp "DROP STATISTICS" $> DropStatistics)
            "CONVERSION" -> step (requireDialectP [PostgreSQL] sp "DROP CONVERSION" $> DropConversion)
            "POLICY" -> step (requireDialectP [PostgreSQL] sp "DROP POLICY" $> DropPolicy)
            "RULE" -> step (requireDialectP [PostgreSQL] sp "DROP RULE" $> DropRule)
            "MACRO" -> step (requireFeatureP FeatureCreateMacro sp "DROP MACRO" $> DropMacro)
            _ ->
                unexpectedUnsupported
                    objTok
                    ("DROP " <> decodeBS upper <> " is not supported")

-- ---------------------------------------------------------------------------
-- ALTER
-- ---------------------------------------------------------------------------

-- | Parse ALTER TABLE or ALTER VIEW with one or more actions.
parseAlter :: P (Located AlterStmt)
parseAlter = withLocated do
    _ <- expectWordCI "ALTER"
    objectKind <-
        matchAnyWordCI ["TABLE", "VIEW"] >>= \case
            Just (_, word)
                | asciiEqCI "VIEW" word -> do
                    sp <- currentSpan
                    requireDialectP [PostgreSQL, MySQL, MariaDB] sp "ALTER VIEW"
                    pure AlterObjectView
                | otherwise -> pure AlterObjectTable
            Nothing ->
                peekTok
                    >>= maybe
                        (unexpectedHere "expected TABLE or VIEW after ALTER")
                        (`unexpectedUnsupported` "only ALTER TABLE and ALTER VIEW are supported")
    ifExists <- parseIfExists
    only <- case objectKind of
        AlterObjectTable -> isJust <$> matchWordCI "ONLY"
        AlterObjectView -> pure False
    table <- parseName
    validatePgRelationName table
    actions <- case objectKind of
        AlterObjectTable -> parseCommaSeparated parseAlterAction
        AlterObjectView -> (: []) <$> parseAlterViewAction
    settings <- case objectKind of
        AlterObjectTable ->
            fromMaybe [] <$> whenWordTok "SETTINGS" \tok -> do
                requireFeatureP FeaturePrewhereSettings (locSpan tok) "ALTER TABLE ... SETTINGS"
                parseCommaSeparated parseSettingItem
        AlterObjectView -> pure []
    pg <- isDialect PostgreSQL
    case actions of
        firstAct : _ : _
            | pg && any isPgStandaloneRename actions ->
                throwParse
                    ParseUnsupported
                    (locSpan firstAct)
                    "PostgreSQL ALTER TABLE RENAME cannot be combined with other actions"
        _ -> pure ()
    pure (AlterStmt objectKind ifExists only table actions settings)
    where
        isPgStandaloneRename (L _ (AlterRenameTable _)) = True
        isPgStandaloneRename (L _ (AlterRenameColumn _ _)) = True
        isPgStandaloneRename _ = False

parseAlterViewAction :: P (Located AlterAction)
parseAlterViewAction = withLocated $ evalContT do
    onWordTok "RENAME" \tok -> do
        requireDialectP [PostgreSQL] (locSpan tok) "ALTER VIEW ... RENAME"
        evalContT do
            onWord "COLUMN" (AlterRenameColumn <$> parseNamePart <*> (expectWordCI "TO" *> parseNamePart))
            lift (expectWordCI "TO" *> (AlterRenameTable <$> parseName))
    onWordTok "ALTER" \tok -> do
        requireDialectP [PostgreSQL] (locSpan tok) "ALTER VIEW ... ALTER COLUMN"
        _ <- matchWordCI "COLUMN"
        namePart <- parseNamePart
        evalContT do
            onWord "SET" (expectWordCI "DEFAULT" *> (AlterAlterColumn namePart . AlterSetDefault <$> parseExpr))
            lift (expectWordsCI ["DROP", "DEFAULT"] $> AlterAlterColumn namePart AlterDropDefault)
    onWordTok "SET" \tok -> do
        requireDialectP [PostgreSQL] (locSpan tok) "ALTER VIEW ... SET"
        AlterViewSetOptions <$> parseParenList parseSettingItem
    onWordTok "RESET" \tok -> do
        requireDialectP [PostgreSQL] (locSpan tok) "ALTER VIEW ... RESET"
        AlterViewResetOptions <$> parseParenList parseNamePart
    onWordTok "OWNER" \tok -> do
        requireDialectP [PostgreSQL] (locSpan tok) "ALTER VIEW ... OWNER TO"
        expectWordCI "TO" *> (AlterOwnerTo <$> parseName)
    onWordTok "AS" \tok -> do
        requireDialectP [MySQL, MariaDB] (locSpan tok) "ALTER VIEW ... AS query"
        AlterViewAsQuery <$> parseQuery
    lift (unexpectedHere "expected an ALTER VIEW action")

parseAlterAction :: P (Located AlterAction)
parseAlterAction = withLocated $ evalContT do
    onWord "ADD" parseAlterAdd
    onWord "DROP" parseAlterDrop
    onWord "RENAME" parseAlterRename
    onWord "ALTER" parseAlterColumn
    onWordTok "MODIFY" \tok -> evalContT do
        onWordTok "SETTING" \settingTok -> do
            requireDialectP [ClickHouse] (locSpan settingTok) "ALTER TABLE ... MODIFY SETTING"
            AlterModifySetting <$> parseCommaSeparated parseSettingItem
        lift do
            requireDialectP [MySQL, MariaDB, ClickHouse] (locSpan tok) "ALTER TABLE ... MODIFY"
            _ <- matchWordCI "COLUMN"
            mysqlLike <- (||) <$> isDialect MySQL <*> isDialect MariaDB
            col <- parseColumnDefWith (ColumnDefOpts mysqlLike (not mysqlLike))
            pos <- if mysqlLike then parseOptionalColumnPosition else pure Nothing
            pure (AlterModifyColumn [col] pos)
    onWordTok "CHANGE" \tok -> do
        requireDialectP [MySQL, MariaDB] (locSpan tok) "ALTER TABLE ... CHANGE"
        _ <- matchWordCI "COLUMN"
        oldName <- parseNamePart
        col <- parseColumnDefWith (ColumnDefOpts True False)
        AlterChangeColumn oldName col <$> parseOptionalColumnPosition
    onWordTok "MATERIALIZE" \tok -> do
        requireDialectP [ClickHouse] (locSpan tok) "ALTER TABLE ... MATERIALIZE TTL"
        expectWordCI "TTL" $> AlterMaterializeTtl
    onWordTok "ATTACH" \tok -> do
        requireFeatureP FeatureAlterPartitionInherit (locSpan tok) "ALTER TABLE ... ATTACH PARTITION"
        _ <- expectWordCI "PARTITION"
        part <- parseName
        bound <- evalContT do
            onWord "DEFAULT" (pure PartitionBoundDefault)
            lift do
                _ <- expectWordsCI ["FOR", "VALUES"]
                parsePartitionBoundSpec
        pure (AlterAttachPartition part bound)
    onWordTok "DETACH" \tok -> do
        requireFeatureP FeatureAlterPartitionInherit (locSpan tok) "ALTER TABLE ... DETACH PARTITION"
        _ <- expectWordCI "PARTITION"
        part <- parseName
        concurrently <- isJust <$> matchWordCI "CONCURRENTLY"
        finalize <- isJust <$> matchWordCI "FINALIZE"
        pure (AlterDetachPartition part concurrently finalize)
    onWordTok "INHERIT" \tok -> do
        requireFeatureP FeatureAlterPartitionInherit (locSpan tok) "ALTER TABLE ... INHERIT"
        parent <- parseName
        validatePgRelationName parent
        pure (AlterInherit parent)
    onWordTok "OWNER" \tok -> do
        requireDialectP [PostgreSQL] (locSpan tok) "ALTER TABLE ... OWNER TO"
        expectWordCI "TO" *> (AlterOwnerTo <$> parseName)
    onWordTok "SET" \tok -> do
        requireDialectP [PostgreSQL] (locSpan tok) "ALTER TABLE ... SET"
        evalContT do
            onWord "LOGGED" (pure (AlterSetLogged True))
            onWord "UNLOGGED" (pure (AlterSetLogged False))
            lift (unexpectedHere "expected LOGGED or UNLOGGED after ALTER TABLE ... SET")
    onWordTok "UPDATE" \tok -> do
        requireDialectP [ClickHouse] (locSpan tok) "ALTER TABLE ... UPDATE"
        assignments <- parseAlterAssignmentList
        partitions <-
            fromMaybe [] <$> whenWord "IN" (expectWordCI "PARTITION" *> parseCommaSeparated parseExpr)
        _ <- expectWordCI "WHERE"
        AlterUpdate assignments partitions <$> parseExpr
    onWordTok "DELETE" \tok -> do
        requireDialectP [ClickHouse] (locSpan tok) "ALTER TABLE ... DELETE"
        partitions <-
            fromMaybe [] <$> whenWord "IN" (expectWordCI "PARTITION" *> parseCommaSeparated parseExpr)
        _ <- expectWordCI "WHERE"
        AlterDelete partitions <$> parseExpr
    onWordTok "ENABLE" (`pgRls` RlsEnable)
    onWordTok "DISABLE" (`pgRls` RlsDisable)
    onWordTok "FORCE" (`pgRls` RlsForce)
    onWordTok "NO" \tok -> evalContT do
        onWord "INHERIT" do
            requireFeatureP FeatureAlterPartitionInherit (locSpan tok) "ALTER TABLE ... NO INHERIT"
            parent <- parseName
            validatePgRelationName parent
            pure (AlterNoInherit parent)
        lift (expectWordCI "FORCE" *> pgRls tok RlsNoForce)
    lift $
        currentWord >>= \case
            Nothing -> unexpectedHere "expected an ALTER TABLE action"
            Just (tok, raw) -> do
                mysql <- isDialect MySQL
                mariadb <- isDialect MariaDB
                if (mysql || mariadb) && wordIn mysqlAlterOptionWords raw
                    then AlterTableOption <$> parseTableOption
                    else throwParse ParseUnsupported (locSpan tok) ("unsupported ALTER TABLE action " <> decodeBS raw)
    where
        pgRls tok act = do
            requireDialectP [PostgreSQL] (locSpan tok) "ROW LEVEL SECURITY"
            expectWordsCI ["ROW", "LEVEL", "SECURITY"] $> AlterRowLevelSecurity act
        mysqlAlterOptionWords =
            [ "ALGORITHM"
            , "LOCK"
            , "ENGINE"
            , "CHARSET"
            , "CHARACTER"
            , "DEFAULT"
            , "COLLATE"
            , "ROW_FORMAT"
            , "AUTO_INCREMENT"
            , "COMMENT"
            , "KEY_BLOCK_SIZE"
            , "PACK_KEYS"
            , "STATS_PERSISTENT"
            , "STATS_AUTO_RECALC"
            , "DELAY_KEY_WRITE"
            , "COMPRESSION"
            ]

parseAlterAssignmentList :: P [Located Assignment]
parseAlterAssignmentList = parseCommaSeparated parseAlterAssignment

parseAlterAssignment :: P (Located Assignment)
parseAlterAssignment = withLocated do
    target <- parseName
    _ <- expectTok TokEq
    Assignment target <$> parseExpr

parseAlterAdd :: P AlterAction
parseAlterAdd = evalContT do
    onWord "COLUMN" parseAlterAddColumns
    lift do
        mt <- peekToken
        case mt of
            Just TokLParen -> parseAlterAddColumns
            Just (TokWord _) ->
                currentHeadWord >>= \case
                    Just w
                        | wordIn ["CONSTRAINT", "PRIMARY", "FOREIGN", "CHECK", "UNIQUE", "FULLTEXT", "LIKE"] w -> do
                            rejectSQLiteAlterAdd
                            AlterAddConstraint <$> parseTableConstraint
                        | wordIn ["INDEX", "KEY"] w -> do
                            rejectSQLiteAlterAdd
                            sp <- currentSpan
                            requireDialectP [MySQL, MariaDB] sp "ALTER TABLE ... ADD INDEX"
                            uncurry AlterAddIndex <$> parseAlterAddIndexBody
                    _ -> parseAlterAddColumns
            _ -> parseAlterAddColumns
    where
        rejectSQLiteAlterAdd = do
            sqlite <- isDialect SQLite
            when sqlite $ do
                sp <- currentSpan
                throwParse
                    ParseUnsupported
                    sp
                    "SQLite supports only ALTER TABLE ... ADD COLUMN"

parseAlterAddColumns :: P AlterAction
parseAlterAddColumns = do
    _ <- matchWordCI "COLUMN"
    ifNotExists <- parseIfNotExists
    mt <- peekToken
    case mt of
        Just TokLParen -> do
            cols <- parseParenList parseColumnDef
            pure (AlterAddColumn ifNotExists cols Nothing)
        _ -> do
            mysqlLike <- (||) <$> isDialect MySQL <*> isDialect MariaDB
            col <- parseColumnDefWith (ColumnDefOpts mysqlLike True)
            pos <- if mysqlLike then parseOptionalColumnPosition else pure Nothing
            pure (AlterAddColumn ifNotExists [col] pos)

parseOptionalColumnPosition :: P (Maybe (Located ColumnPosition))
parseOptionalColumnPosition = evalContT do
    onWordTok "FIRST" \tok -> pure (Just (L (locSpan tok) ColumnPositionFirst))
    onWordTok "AFTER" \tok -> do
        col <- parseNamePart
        Just <$> locatedSpanning (locSpan tok) (ColumnPositionAfter col)
    pure Nothing

parseAlterAddIndexBody :: P (Maybe (Located NamePart), [Located IndexKeyPart])
parseAlterAddIndexBody = do
    _ <- matchAnyWordCI ["INDEX", "KEY"] >>= maybe (unexpectedHere "expected INDEX or KEY") pure
    name <- parseOptionalConstraintName
    keys <- parseIndexKeyList
    pure (name, keys)

parseAlterDrop :: P AlterAction
parseAlterDrop = evalContT do
    onWord "COLUMN" (AlterDropColumn <$> parseIfExists <*> parseNamePart <*> parseDropBehavior)
    onWordTok "CONSTRAINT" \tok -> do
        rejectSQLiteAlterDrop tok
        AlterDropConstraint <$> parseIfExists <*> parseNamePart <*> parseDropBehavior
    onWordTok "INDEX" \tok -> do
        rejectSQLiteAlterDrop tok
        AlterDropIndex <$> parseIfExists <*> parseNamePart <*> parseDropBehavior
    onWordTok "KEY" \tok -> do
        rejectSQLiteAlterDrop tok
        AlterDropIndex <$> parseIfExists <*> parseNamePart <*> parseDropBehavior
    onWordTok "PRIMARY" \tok -> do
        rejectSQLiteAlterDrop tok
        expectWordCI "KEY" $> AlterDropPrimaryKey
    onWordTok "FOREIGN" \tok -> do
        rejectSQLiteAlterDrop tok
        expectWordCI "KEY" *> (AlterDropConstraint False <$> parseNamePart <*> parseDropBehavior)
    lift (AlterDropColumn <$> parseIfExists <*> parseNamePart <*> parseDropBehavior)
    where
        rejectSQLiteAlterDrop tok = do
            sqlite <- isDialect SQLite
            when sqlite $
                throwParse
                    ParseUnsupported
                    (locSpan tok)
                    "SQLite supports only ALTER TABLE ... DROP [COLUMN]"

parseAlterRename :: P AlterAction
parseAlterRename = evalContT do
    onWord "COLUMN" renameCol
    onWord "TO" (AlterRenameTable <$> parseName)
    lift renameCol
    where
        renameCol = AlterRenameColumn <$> parseNamePart <*> (expectWordCI "TO" *> parseNamePart)

parseAlterColumn :: P AlterAction
parseAlterColumn = do
    _ <- matchWordCI "COLUMN"
    namePart <- parseNamePart
    action <- evalContT do
        onWord "SET" $ evalContT do
            onWord "DEFAULT" (AlterSetDefault <$> parseExpr)
            onWord "NOT" (expectWordCI "NULL" $> AlterSetNotNull)
            onWord "DATA" (expectWordCI "TYPE" *> parseAlterType)
            lift (expectWordCI "TYPE" *> parseAlterType)
        onWord "DROP" $ evalContT do
            onWord "DEFAULT" (pure AlterDropDefault)
            lift (expectWordsCI ["NOT", "NULL"] $> AlterDropNotNull)
        onWord "TYPE" parseAlterType
        onWordTok "ADD" \tok -> do
            requireDialectP [PostgreSQL] (locSpan tok) "ALTER COLUMN ... ADD GENERATED AS IDENTITY"
            _ <- expectWordCI "GENERATED"
            isAlways <- evalContT do
                onWord "ALWAYS" (pure True)
                onWord "BY" (expectWordCI "DEFAULT" $> False)
                lift (unexpectedHere "expected ALWAYS or BY DEFAULT after GENERATED")
            expectWordsCI ["AS", "IDENTITY"]
            opts <- whenTok TokLParen (parseTableOptionList [] <* expectTok TokRParen)
            pure (AlterAddIdentity isAlways (maybe [] reverse opts))
        onWordTok "OPTIONS" \tok -> do
            requireDialectP [PostgreSQL] (locSpan tok) "ALTER COLUMN ... OPTIONS"
            AlterColumnOptions <$> parseParenList parseGenericOption
        lift (unexpectedHere "expected SET, DROP, TYPE, ADD, or OPTIONS after ALTER COLUMN name")
    pure (AlterAlterColumn namePart action)
    where
        parseAlterType = do
            ty <- parseTypeName
            usingExpr <- whenWordTok "USING" \tok -> do
                requireDialectP [PostgreSQL] (locSpan tok) "ALTER COLUMN ... TYPE ... USING"
                parseExpr
            pure (AlterType ty usingExpr)

parseGenericOption :: P (Located GenericOption)
parseGenericOption = withLocated do
    action <- fmap (\(L sp _, w) -> L sp (asciiUpperBS w)) <$> matchAnyWordCI ["ADD", "SET", "DROP"]
    optName <- parseNamePart
    val <- case fmap locValue action of
        Just "DROP" -> pure Nothing
        _ -> Just <$> parseExpr
    pure (GenericOption action optName val)

-- | Parse an optional @CASCADE@ or @RESTRICT@.
parseDropBehavior :: P (Maybe DropBehavior)
parseDropBehavior = evalContT do
    onWord "CASCADE" (pure (Just DropCascade))
    onWord "RESTRICT" (pure (Just DropRestrict))
    pure Nothing
