-- | CREATE, DROP, ALTER, and column/table-definition parsers.
module Occam.Frontend.Parser.DDL (
    parseCreate,
    parseDrop,
    parseAlter,
    parseIfNotExists,
    parseIfExists,
    parseDropBehavior,
    parseColumnDef,
    parseTableConstraint,
    parseTableOption,
) where

import Control.Applicative ((<|>))
import Control.Monad (join, unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.State.Strict (StateT, execStateT, modify')
import Data.ByteString (ByteString)
import Data.Foldable (find, traverse_)
import Data.Functor (($>))
import Data.HashSet qualified as HashSet
import Data.Maybe (fromMaybe, isJust, isNothing)
import Occam.Frontend.AST
import Occam.Frontend.Lexer hiding (LiteralKind)
import Occam.Frontend.Parser.Core
import Occam.Frontend.Parser.Expr
import Occam.Frontend.Parser.Query
import Occam.Frontend.Span (ByteSpan (..), Located (..), mergeSpan, spanIsEmpty, spanStart)
import Occam.Parser.Dialect (DialectFeature (..))
import Occam.Parser.Types (SQLDialect (..))

-- ---------------------------------------------------------------------------
-- CREATE
-- ---------------------------------------------------------------------------

data CreateFlags = CreateFlags
    { cfOrReplace :: !(Maybe ByteSpan)
    , cfTemp :: !(Maybe ByteSpan)
    , cfUnique :: !(Maybe ByteSpan)
    , cfFulltext :: !(Maybe ByteSpan)
    , cfMaterialized :: !(Maybe ByteSpan)
    , cfVirtual :: !(Maybe ByteSpan)
    , cfAlgorithm :: !(Maybe (Located ByteString))
    }

emptyCreateFlags :: CreateFlags
emptyCreateFlags = CreateFlags Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Parse a CREATE statement and its dialect-specific object form.
parseCreate :: P (Located CreateStmt)
parseCreate = withLocated do
    _ <- expectWordCI "CREATE"
    flags <- parseCreateFlags emptyCreateFlags
    (objTok, objRaw) <-
        currentWord >>= maybe (unexpectedHere "expected an object type after CREATE") pure
    let upper = asciiUpperBS objRaw
    validateCreateFlags flags upper
    obj <- parseCreateObject upper objTok flags
    pure
        CreateStmt
            { createOrReplace = isJust (cfOrReplace flags)
            , createTemp = isJust (cfTemp flags)
            , createUnique = isJust (cfUnique flags)
            , createFulltext = isJust (cfFulltext flags)
            , createAlgorithm = cfAlgorithm flags
            , createObject = obj
            }

parseCreateObject :: ByteString -> SpannedToken -> CreateFlags -> P (Located CreateObject)
parseCreateObject upper objTok flags =
    let sp = locSpan objTok
        build p = advanceTok *> p >>= locatedSpanning sp
     in case upper of
            "TABLE" ->
                build $
                    if isJust (cfVirtual flags)
                        then CreateVirtualTableObject <$> parseCreateVirtualTable
                        else CreateTableObject <$> parseCreateTable
            "INDEX" ->
                build
                    ( requireDialectP [PostgreSQL, MySQL, MariaDB, SQLite, DuckDB] sp "CREATE INDEX"
                        *> (CreateIndexObject <$> parseCreateIndex)
                    )
            "VIEW" -> build (CreateViewObject <$> parseCreateView (isJust (cfMaterialized flags)))
            "SEQUENCE" ->
                build
                    ( requireFeatureP FeatureSequenceDDL sp "CREATE SEQUENCE"
                        *> (CreateSequenceObject <$> parseCreateSequence)
                    )
            "SCHEMA" ->
                build
                    ( requireDialectP [PostgreSQL, MySQL, MariaDB, DuckDB] sp "CREATE SCHEMA"
                        *> (CreateSchemaObject <$> parseCreateSchema)
                    )
            "DATABASE" ->
                build
                    ( requireDialectP [PostgreSQL, MySQL, MariaDB, ClickHouse] sp "CREATE DATABASE"
                        *> (CreateDatabaseObject <$> parseCreateDatabase)
                    )
            "STATISTICS" ->
                build
                    ( requireFeatureP FeatureCreateStatistics sp "CREATE STATISTICS"
                        *> (CreateStatisticsObject <$> parseCreateStatistics)
                    )
            "MACRO" ->
                build
                    (requireFeatureP FeatureCreateMacro sp "CREATE MACRO" *> (CreateMacroObject <$> parseCreateMacro))
            "POLICY" ->
                build
                    (requireDialectP [PostgreSQL] sp "CREATE POLICY" *> (CreatePolicyObject <$> parseCreatePolicy))
            "RULE" ->
                build (requireDialectP [PostgreSQL] sp "CREATE RULE" *> (CreateRuleObject <$> parseCreateRule))
            "TRIGGER" ->
                build
                    ( requireDialectP [PostgreSQL, MySQL, MariaDB, SQLite] sp "CREATE TRIGGER"
                        *> (CreateTriggerObject <$> parseCreateTrigger)
                    )
            "FUNCTION" ->
                build
                    ( requireDialectP [PostgreSQL, MySQL, MariaDB, ClickHouse] sp "CREATE FUNCTION"
                        *> (CreateFunctionObject <$> parseCreateRoutine False)
                    )
            "PROCEDURE" ->
                build
                    ( requireDialectP [PostgreSQL, MySQL, MariaDB] sp "CREATE PROCEDURE"
                        *> (CreateProcedureObject <$> parseCreateRoutine True)
                    )
            "AGGREGATE" ->
                build
                    ( requireDialectP [PostgreSQL] sp "CREATE AGGREGATE"
                        *> (CreateAggregateObject <$> parseCreateAggregate)
                    )
            "EXTENSION" ->
                build
                    ( requireDialectP [PostgreSQL] sp "CREATE EXTENSION"
                        *> (CreateExtensionObject <$> parseCreateExtension)
                    )
            _ ->
                unexpectedUnsupported
                    objTok
                    ("CREATE " <> decodeBS upper <> " is not supported")

parseCreateFlags :: CreateFlags -> P CreateFlags
parseCreateFlags flags = do
    mOr <- wordIsCurrent "OR"
    mReplace <- wordIsAt 1 "REPLACE"
    if mOr && mReplace
        then do
            orTok <- expectWordCI "OR"
            _ <- expectWordCI "REPLACE"
            parseCreateFlags flags {cfOrReplace = Just (locSpan orTok)}
        else evalContT do
            onWords ["TEMP", "TEMPORARY"] \tok hit -> do
                when (asciiEqCI "TEMP" hit) $ do
                    mysqlLike <- (||) <$> isDialect MySQL <*> isDialect MariaDB
                    when mysqlLike $
                        throwParse
                            ParseUnsupported
                            (locSpan tok)
                            "MySQL/MariaDB require the TEMPORARY spelling, not TEMP"
                parseCreateFlags flags {cfTemp = Just (locSpan tok)}
            onWordTok "UNIQUE" \tok ->
                parseCreateFlags flags {cfUnique = Just (locSpan tok)}
            onWordTok "FULLTEXT" \tok ->
                parseCreateFlags flags {cfFulltext = Just (locSpan tok)}
            onWordTok "MATERIALIZED" \tok ->
                parseCreateFlags flags {cfMaterialized = Just (locSpan tok)}
            onWordTok "VIRTUAL" \tok ->
                parseCreateFlags flags {cfVirtual = Just (locSpan tok)}
            onWordTok "ALGORITHM" \tok -> do
                requireDialectP [MySQL, MariaDB] (locSpan tok) "CREATE ALGORITHM"
                _ <- matchTok TokEq
                (valTok, valRaw) <-
                    currentWord >>= maybe (unexpectedHere "expected algorithm name after ALGORITHM") pure
                _ <- advanceTok
                parseCreateFlags flags {cfAlgorithm = Just (L (locSpan valTok) valRaw)}
            lift $
                currentWord >>= \case
                    Just (_, raw)
                        | wordIn ["GLOBAL", "LOCAL"] raw -> do
                            isTemp <- (||) <$> wordIsAt 1 "TEMP" <*> wordIsAt 1 "TEMPORARY"
                            if isTemp
                                then do
                                    _ <- advanceTok
                                    tempTok <- advanceTok
                                    parseCreateFlags flags {cfTemp = Just (locSpan tempTok)}
                                else pure flags
                    _ -> pure flags

validateCreateFlags :: CreateFlags -> ByteString -> P ()
validateCreateFlags flags objRaw = do
    traverse_ checkOrReplace (cfOrReplace flags)
    traverse_ checkTemp (cfTemp flags)
    traverse_ checkMat (cfMaterialized flags)
    traverse_ checkUnique (cfUnique flags)
    traverse_ checkFulltext (cfFulltext flags)
    traverse_ checkVirtual (cfVirtual flags)
    traverse_ checkAlgorithm (cfAlgorithm flags)
    where
        checkOrReplace sp = case objRaw of
            "TABLE" -> requireDialectP [DuckDB, ClickHouse, MariaDB] sp "CREATE OR REPLACE TABLE"
            "VIEW"
                | isJust (cfMaterialized flags) ->
                    requireDialectP [ClickHouse] sp "CREATE OR REPLACE MATERIALIZED VIEW"
                | otherwise ->
                    requireDialectP [PostgreSQL, MySQL, MariaDB, DuckDB, ClickHouse] sp "CREATE OR REPLACE VIEW"
            "FUNCTION" -> requireDialectP [PostgreSQL, MariaDB] sp "CREATE OR REPLACE FUNCTION"
            "PROCEDURE" -> requireDialectP [PostgreSQL, MariaDB] sp "CREATE OR REPLACE PROCEDURE"
            "AGGREGATE" -> requireDialectP [PostgreSQL] sp "CREATE OR REPLACE AGGREGATE"
            "TRIGGER" -> requireDialectP [PostgreSQL, MariaDB] sp "CREATE OR REPLACE TRIGGER"
            "SEQUENCE" -> requireDialectP [MariaDB, DuckDB] sp "CREATE OR REPLACE SEQUENCE"
            "MACRO" -> requireDialectP [DuckDB] sp "CREATE OR REPLACE MACRO"
            "RULE" -> requireDialectP [PostgreSQL] sp "CREATE OR REPLACE RULE"
            _ -> throwParse ParseUnsupported sp ("CREATE OR REPLACE " <> decodeBS objRaw <> " is not supported")
        checkTemp sp = case objRaw of
            "TABLE" -> pure ()
            "VIEW" -> requireDialectP [PostgreSQL, SQLite, DuckDB] sp "TEMP VIEW"
            "SEQUENCE" -> requireDialectP [PostgreSQL, DuckDB, MariaDB] sp "TEMP SEQUENCE"
            "MACRO" -> requireDialectP [DuckDB] sp "TEMP MACRO"
            "POLICY" -> throwParse ParseUnsupported sp "CREATE TEMP POLICY is not valid in PostgreSQL"
            _ -> throwParse ParseUnsupported sp ("CREATE TEMP " <> decodeBS objRaw <> " is not supported")
        checkMat sp
            | objRaw == "VIEW" = requireFeatureP FeatureMaterializedViews sp "CREATE MATERIALIZED VIEW"
            | otherwise = throwParse ParseUnsupported sp "MATERIALIZED is only valid for CREATE VIEW"
        checkUnique sp
            | objRaw == "INDEX" = pure ()
            | otherwise = throwParse ParseUnsupported sp "UNIQUE is only valid for CREATE INDEX"
        checkFulltext sp
            | objRaw == "INDEX" = requireFeatureP FeatureFulltextIndex sp "CREATE FULLTEXT INDEX"
            | otherwise = throwParse ParseUnsupported sp "FULLTEXT is only valid for CREATE INDEX"
        checkVirtual sp
            | objRaw == "TABLE" =
                requireFeatureP FeatureVirtualTables sp "CREATE VIRTUAL TABLE"
            | otherwise = throwParse ParseUnsupported sp "VIRTUAL is only valid for CREATE TABLE"
        checkAlgorithm alg
            | objRaw == "VIEW" = requireDialectP [MySQL, MariaDB] (locSpan alg) "CREATE ALGORITHM VIEW"
            | otherwise = throwParse ParseUnsupported (locSpan alg) "ALGORITHM is only valid for CREATE VIEW"

-- | Consume @IF NOT EXISTS@ and report whether it was present.
parseIfNotExists :: P Bool
parseIfNotExists = isJust <$> whenWord "IF" (expectWordsCI ["NOT", "EXISTS"])

-- | Consume @IF EXISTS@ and report whether it was present.
parseIfExists :: P Bool
parseIfExists = isJust <$> whenWord "IF" (expectWordCI "EXISTS")

data CreateTableTail = CreateTableTail
    { cttOptions :: ![Located TableOption]
    , cttEngine :: !(Maybe OpaqueBody)
    , cttSettings :: ![Located SettingItem]
    , cttPartitionBy :: !(Maybe (Located PartitionBy))
    , cttAs :: !(Maybe (Located Query))
    , cttWithData :: !(Maybe Bool)
    }

parseCreateTable :: P CreateTable
parseCreateTable = do
    ifNotExists <- parseIfNotExists
    name <- parseName
    hasParens <- (== Just TokLParen) <$> peekToken
    elements <- parseOptParenList parseTableElement
    when (hasParens && null elements) $
        requireDialectP [PostgreSQL] (locSpan name) "empty column list in CREATE TABLE"
    partOf <-
        if hasParens
            then pure Nothing
            else whenWordTok "PARTITION" \tok -> do
                requireDialectP [PostgreSQL] (locSpan tok) "PARTITION OF"
                parsePartitionOf (locSpan tok)
    tailInfo <- execStateT parseCreateTableTail (CreateTableTail [] Nothing [] Nothing Nothing Nothing)
    let hasBody =
            hasParens
                || not (null elements)
                || isJust (cttAs tailInfo)
                || isJust (cttEngine tailInfo)
                || isJust partOf
    unless hasBody $
        unexpectedHere "CREATE TABLE requires a column list, PARTITION OF, ENGINE, or AS query"
    pure
        CreateTable
            { createTableName = name
            , createTableIfNotExists = ifNotExists
            , createTableElements = elements
            , createTableOptions = reverse (cttOptions tailInfo)
            , createTableEngine = cttEngine tailInfo
            , createTableSettings = reverse (cttSettings tailInfo)
            , createTablePartitionOf = partOf
            , createTablePartitionBy = cttPartitionBy tailInfo
            , createTableAs = cttAs tailInfo
            , createTableWithData = cttWithData tailInfo
            }

parsePartitionOf :: ByteSpan -> P (Located TablePartition)
parsePartitionOf startSp = do
    _ <- expectWordCI "OF"
    parent <- parseName
    bound <- evalContT do
        onWord "DEFAULT" (pure PartitionBoundDefault)
        lift do
            _ <- expectWordsCI ["FOR", "VALUES"]
            parsePartitionBoundSpec
    locatedSpanning startSp (TablePartition parent bound)

parsePartitionBoundSpec :: P PartitionBound
parsePartitionBoundSpec = evalContT do
    onWord "IN" (PartitionBoundIn <$> parseParenList parseExpr)
    onWord "FROM" do
        froms <- parseParenList parseExpr
        _ <- expectWordCI "TO"
        tos <- parseParenList parseExpr
        pure (PartitionBoundFromTo froms tos)
    onWord "WITH" (PartitionBoundWith <$> parseParenList ((,) <$> parseNamePart <*> parseExpr))
    lift (unexpectedHere "expected IN, FROM, or WITH after FOR VALUES")

parseCreateTableTail :: StateT CreateTableTail P ()
parseCreateTableTail =
    lift currentHeadWord >>= \case
        Just w
            | asciiEqCI "AS" w -> do
                q <- lift (expectWordCI "AS" *> parseQuery)
                withData <- lift parseOptionalWithData
                modify' (\s -> s {cttAs = Just q, cttWithData = withData})
            | asciiEqCI "SETTINGS" w -> do
                items <- lift do
                    tok <- expectWordCI "SETTINGS"
                    requireFeatureP FeaturePrewhereSettings (locSpan tok) "SETTINGS"
                    parseCommaSeparated parseSettingItem
                modify' (\s -> s {cttSettings = reverse items ++ cttSettings s})
                parseCreateTableTail
            | asciiEqCI "PARTITION" w -> do
                pg <- lift (isDialect PostgreSQL)
                if pg
                    then do
                        partBy <- lift $ withLocated do
                            _ <- expectWordCI "PARTITION"
                            _ <- expectWordCI "BY"
                            (stratTok, stratRaw) <-
                                currentWord >>= maybe (unexpectedHere "expected partitioning strategy after PARTITION BY") pure
                            _ <- advanceTok
                            PartitionBy (L (locSpan stratTok) stratRaw) <$> parseParenList parseExpr
                        modify' (\s -> s {cttPartitionBy = Just partBy})
                        parseCreateTableTail
                    else do
                        option <- lift parseTableOption
                        modify' (\s -> s {cttOptions = option : cttOptions s})
                        parseCreateTableTail
            | asciiEqCI "ENGINE" w -> do
                ch <- lift (isDialect ClickHouse)
                if ch
                    then do
                        body <- lift do
                            tok <- expectWordCI "ENGINE"
                            requireFeatureP FeatureEngineClause (locSpan tok) "ENGINE clause"
                            _ <- matchTok TokEq
                            b <- parseOpaqueUntil OpaqueEngineExpression ["AS", "SETTINGS", "TO", "POPULATE"]
                            when (spanIsEmpty (opaqueBodySpan b)) (unexpectedHere "expected an ENGINE expression")
                            pure b
                        modify' (\s -> s {cttEngine = Just body})
                    else do
                        option <- lift do
                            currentWord >>= traverse_ \(engTok, _) ->
                                requireFeatureP FeatureEngineClause (locSpan engTok) "ENGINE clause"
                            parseTableOption
                        modify' (\s -> s {cttOptions = option : cttOptions s})
                parseCreateTableTail
        Nothing -> pure ()
        Just _ ->
            lift peekTok >>= \case
                Just (L _ TokComma) -> do
                    option <- lift (advanceTok *> parseTableOption)
                    modify' (\s -> s {cttOptions = option : cttOptions s})
                    parseCreateTableTail
                Just (L _ (TokWord _)) -> do
                    option <- lift parseTableOption
                    modify' (\s -> s {cttOptions = option : cttOptions s})
                    parseCreateTableTail
                _ -> lift (unexpectedHere "expected table options, AS query, or end of statement")

parseTableElement :: P (Located TableElement)
parseTableElement = do
    mw <- currentHeadWord
    clickhouse <- isDialect ClickHouse
    case mw of
        Just w
            | asciiEqCI "PROJECTION" w -> rewrap TableElementProjection <$> parseProjectionDef
            | clickhouse && asciiEqCI "INDEX" w ->
                rewrap TableElementSkippingIndex <$> parseSkippingIndex
            | startsTableConstraint w -> rewrap TableElementConstraint <$> parseTableConstraint
        _ -> rewrap TableElementColumn <$> parseColumnDef

-- | ClickHouse inline @INDEX name expr TYPE type(...) [GRANULARITY n]@.
parseSkippingIndex :: P (Located SkippingIndex)
parseSkippingIndex = withLocated do
    tok <- expectWordCI "INDEX"
    requireDialectP [ClickHouse] (locSpan tok) "inline data-skipping index"
    namePart <- parseNamePart
    expr <- parseExpr
    _ <- expectWordCI "TYPE"
    typeName <- parseTypeName
    granularity <- whenWord "GRANULARITY" parseExpr
    pure (SkippingIndex namePart expr typeName granularity)

-- | ClickHouse @PROJECTION name (query)@ element.
parseProjectionDef :: P (Located ProjectionDef)
parseProjectionDef = withLocated do
    tok <- expectWordCI "PROJECTION"
    requireDialectP [ClickHouse] (locSpan tok) "PROJECTION"
    namePart <- parseNamePart
    ProjectionDef namePart <$> inParens parseQuery

startsTableConstraint :: ByteString -> Bool
startsTableConstraint =
    wordIn ["CONSTRAINT", "PRIMARY", "UNIQUE", "FOREIGN", "CHECK", "KEY", "INDEX", "FULLTEXT", "LIKE"]

-- | Parse PRIMARY KEY, UNIQUE, FOREIGN KEY, CHECK, KEY/INDEX, FULLTEXT, or LIKE.
parseTableConstraint :: P (Located TableConstraint)
parseTableConstraint = withLocated do
    mConstraint <- whenWord "CONSTRAINT" $ do
        mw <- currentWord
        if maybe False (isTableConstraintKeyword . snd) mw
            then pure Nothing
            else Just <$> parseNamePart
    let cName = join mConstraint
        parseIndexName = do
            using <- wordIsCurrent "USING"
            if using then pure Nothing else parseOptionalConstraintName
        parseMethod = whenWordTok "USING" \tok -> do
            requireDialectP [MySQL, MariaDB] (locSpan tok) "index method"
            parseNamePart
        parseKey ctor = do
            indexName <- parseIndexName
            method <- parseMethod
            keys <- parseIndexKeyList
            trailingMethod <- parseMethod
            when (isJust method && isJust trailingMethod) (unexpectedHere "duplicate index method")
            pure (ctor (ConstraintIndexOptions cName indexName (method <|> trailingMethod)) keys)
        parseUsingIndex isPrimary = do
            usingTok <- expectWordCI "USING"
            requireDialectP [PostgreSQL] (locSpan usingTok) "USING INDEX constraint"
            _ <- expectWordCI "INDEX"
            idxName <- parseNamePart
            _ <- whenWord "NOT" (expectWordCI "DEFERRABLE")
            _ <- matchWordCI "DEFERRABLE"
            _ <- whenWord "INITIALLY" (matchAnyWordCI ["DEFERRED", "IMMEDIATE"])
            pure (TableConstraintUsingIndex cName isPrimary idxName)
    evalContT do
        onWord "PRIMARY" do
            _ <- expectWordCI "KEY"
            isUsingIndex <- (&&) <$> wordIsCurrent "USING" <*> wordIsAt 1 "INDEX"
            if isUsingIndex then parseUsingIndex True else parseKey TableConstraintPrimaryKey
        onWord "UNIQUE" do
            isUsingIndex <- (&&) <$> wordIsCurrent "USING" <*> wordIsAt 1 "INDEX"
            if isUsingIndex
                then parseUsingIndex False
                else matchAnyWordCI ["KEY", "INDEX"] *> parseKey TableConstraintUnique
        onWord "FOREIGN" do
            _ <- expectWordCI "KEY"
            indexName <- parseIndexName
            cols <- parseParenNamePartList
            TableConstraintForeignKey (ConstraintIndexOptions cName indexName Nothing) cols
                <$> parseForeignKeyRef
        onWord "CHECK" (TableConstraintCheck cName <$> inParens parseExpr)
        onWords ["KEY", "INDEX"] \_ _ -> parseKey TableConstraintIndex
        onWordTok "FULLTEXT" \tok -> do
            requireFeatureP FeatureFulltextIndex (locSpan tok) "FULLTEXT index"
            _ <- matchAnyWordCI ["KEY", "INDEX"]
            parseKey TableConstraintIndex
        onWord "LIKE" (TableConstraintLike <$> parseName <*> parseTableLikeOptions)
        lift (unexpectedHere "expected a table constraint")

isTableConstraintKeyword :: ByteString -> Bool
isTableConstraintKeyword =
    wordIn ["PRIMARY", "UNIQUE", "FOREIGN", "CHECK", "KEY", "INDEX", "FULLTEXT", "LIKE"]

parseTableLikeOptions :: P [Located TableLikeOption]
parseTableLikeOptions = go []
    where
        go acc =
            matchAnyWordCI ["INCLUDING", "EXCLUDING"] >>= \case
                Nothing -> pure (reverse acc)
                Just (_, directive) -> do
                    optionTok <- advanceTok
                    let optionName = L (locSpan optionTok) (asciiUpperBS (tokenRaw optionTok))
                    go (L (locSpan optionTok) (TableLikeOption (asciiEqCI "INCLUDING" directive) optionName) : acc)

parseParenNamePartList :: P [Located NamePart]
parseParenNamePartList = parseParenList parseNamePart

-- | Parse @name [type] [constraints]@, optionally with ClickHouse SETTINGS.
parseColumnDef :: P (Located ColumnDef)
parseColumnDef = parseColumnDefWith False

parseColumnDefWith :: Bool -> P (Located ColumnDef)
parseColumnDefWith allowPosition = withLocated do
    namePart <- parseNamePart
    mt <- peekTok
    mType <- if tokenStartsType mt then Just <$> parseTypeName else pure Nothing
    constraints <- parseColumnConstraints allowPosition []
    clickhouse <- isDialect ClickHouse
    settings <-
        if clickhouse
            then fromMaybe [] <$> whenWord "SETTINGS" (parseCommaSeparated parseSettingItem)
            else pure []
    pure (ColumnDef namePart mType constraints settings)

tokenStartsType :: Maybe SpannedToken -> Bool
tokenStartsType (Just (L _ (TokWord wt))) =
    maybe True (not . wordIn columnConstraintStarters . wordRaw) (unquotedWord wt)
tokenStartsType _ = False

columnConstraintStarters :: [ByteString]
columnConstraintStarters =
    [ "CONSTRAINT"
    , "PRIMARY"
    , "NOT"
    , "NULL"
    , "UNIQUE"
    , "CHECK"
    , "DEFAULT"
    , "REFERENCES"
    , "COLLATE"
    , "GENERATED"
    , "AUTOINCREMENT"
    , "AUTO_INCREMENT"
    , "ON"
    , "COMMENT"
    , "AS"
    ]

parseColumnConstraints :: Bool -> [Located ColumnConstraint] -> P [Located ColumnConstraint]
parseColumnConstraints allowPosition acc =
    peekTok >>= \case
        Nothing -> pure (reverse acc)
        Just (L _ TokComma) -> pure (reverse acc)
        Just (L _ TokRParen) -> pure (reverse acc)
        Just (L _ (TokWord wt))
            | allowPosition
            , Just uw <- unquotedWord wt
            , wordIn ["FIRST", "AFTER"] (wordRaw uw) ->
                pure (reverse acc)
            | Just uw <- unquotedWord wt
            , asciiEqCI "SETTINGS" (wordRaw uw) -> do
                ch <- isDialect ClickHouse
                if ch
                    then pure (reverse acc)
                    else unexpectedHere "expected a column constraint, ',' or ')'"
            | Just uw <- unquotedWord wt
            , asciiEqCI "EPHEMERAL" (wordRaw uw) -> do
                ch <- isDialect ClickHouse
                if ch
                    then do
                        constraint <- parseColumnConstraint
                        parseColumnConstraints allowPosition (constraint : acc)
                    else unexpectedHere "expected a column constraint, ',' or ')'"
            | Just uw <- unquotedWord wt
            , wordIn (columnConstraintStarters ++ ["CHARACTER", "BINARY", "KEY"]) (wordRaw uw) -> do
                constraint <- parseColumnConstraint
                parseColumnConstraints allowPosition (constraint : acc)
        _ -> unexpectedHere "expected a column constraint, ',' or ')'"

parseColumnConstraint :: P (Located ColumnConstraint)
parseColumnConstraint = do
    start <- currentSpan
    locatedSpanning start =<< evalContT do
        onWordTok "CONSTRAINT" \tok -> do
            requireNonClickHouseColumnConstraint tok "named column constraint"
            namePart <- parseNamePart
            ColumnConstraintNamed namePart . locValue <$> parseColumnConstraint
        onWord "NOT" do
            matchWordCI "NULL"
                >>= maybe
                    (throwParse ParseUnsupported start "only NOT NULL column constraints are supported")
                    (const (withConflict ColumnConstraintNotNull))
        onWord "NULL" (withConflict ColumnConstraintNull)
        onWord "DEFAULT" (ColumnConstraintDefault <$> parseExpr)
        onWordTok "PRIMARY" \tok -> do
            requireNonClickHouseColumnConstraint tok "column PRIMARY KEY constraint"
            _ <- expectWordCI "KEY"
            ac <- currentHeadWord
            when (maybe False (wordIn ["ASC", "DESC"]) ac) $ do
                sp <- currentSpan
                requireDialectP [SQLite] sp "sorted column PRIMARY KEY"
            direction <- parseSortDirection
            withConflict (ColumnConstraintPrimaryKey direction)
        onWordTok "UNIQUE" \tok -> do
            requireNonClickHouseColumnConstraint tok "column UNIQUE constraint"
            matchWordCI "KEY" *> withConflict ColumnConstraintUnique
        onWordTok "CHECK" \tok -> do
            requireNonClickHouseColumnConstraint tok "column CHECK constraint"
            ColumnConstraintCheck <$> inParens parseExpr
        onWordTok "REFERENCES" \tok -> do
            requireNonClickHouseColumnConstraint tok "column REFERENCES constraint"
            ColumnConstraintReferences <$> (locatedSpanning (locSpan tok) =<< parseForeignKeyRefAfter)
        onWord "COLLATE" (ColumnConstraintCollate <$> parseNameOrLiteralRaw)
        onWordTok "KEY" \tok ->
            requireDialectP [MySQL, MariaDB] (locSpan tok) "column KEY attribute" $> ColumnConstraintKey
        onWord "BINARY" (pure ColumnConstraintBinary)
        onWords ["AUTO_INCREMENT", "AUTOINCREMENT"] \tok _ -> do
            requireNonClickHouseColumnConstraint tok "AUTO_INCREMENT"
            pure ColumnConstraintAutoIncrement
        onWordTok "GENERATED" \tok -> do
            requireNonClickHouseColumnConstraint tok "GENERATED column"
            isAlways <- evalContT do
                onWord "ALWAYS" (pure True)
                onWord "BY" (expectWordCI "DEFAULT" $> False)
                lift (unexpectedHere "expected ALWAYS or BY DEFAULT after GENERATED")
            _ <- expectWordCI "AS"
            evalContT do
                onWordTok "IDENTITY" \idTok -> do
                    requireDialectP [PostgreSQL] (locSpan idTok) "GENERATED ... AS IDENTITY"
                    opts <- whenTok TokLParen (parseTableOptionList [] <* expectTok TokRParen)
                    pure (ColumnConstraintIdentity isAlways (maybe [] reverse opts))
                lift do
                    unless isAlways $
                        unexpectedHere "GENERATED BY DEFAULT requires AS IDENTITY"
                    parseGeneratedBody
        onWord "AS" parseGeneratedBody
        onWordTok "ON" \tok -> do
            requireDialectP [MySQL, MariaDB] (locSpan tok) "column ON UPDATE"
            expectWordCI "UPDATE" *> (ColumnConstraintOnUpdate <$> parseExpr)
        onWord "COMMENT" (ColumnConstraintComment <$> parseExpr)
        onWord "CHARACTER" (expectWordCI "SET" *> (ColumnConstraintCharacterSet <$> parseNameOrLiteralRaw))
        onWordTok "EPHEMERAL" \tok -> do
            requireDialectP [ClickHouse] (locSpan tok) "EPHEMERAL column"
            hasExpr <-
                peekTok >>= \case
                    Nothing -> pure False
                    Just (L _ TokComma) -> pure False
                    Just (L _ TokRParen) -> pure False
                    Just (L _ (TokWord wt))
                        | Just uw <- unquotedWord wt
                        , wordIn
                            (columnConstraintStarters ++ ["SETTINGS", "CODEC", "TTL", "EPHEMERAL", "CHARACTER", "BINARY", "KEY"])
                            (wordRaw uw) ->
                            pure False
                    _ -> pure True
            ColumnConstraintEphemeral <$> if hasExpr then Just <$> parseExpr else pure Nothing
        lift (unexpectedHere "expected a column constraint")
    where
        requireNonClickHouseColumnConstraint tok =
            requireDialectP [PostgreSQL, MySQL, MariaDB, SQLite, DuckDB] (locSpan tok)
        withConflict constraint = do
            sqlite <- isDialect SQLite
            onConflict <- (&&) <$> wordIsCurrent "ON" <*> wordIsAt 1 "CONFLICT"
            if sqlite && onConflict
                then do
                    expectWordsCI ["ON", "CONFLICT"]
                    action <- withLocated $ evalContT do
                        onWord "ROLLBACK" (pure ColumnConflictRollback)
                        onWord "ABORT" (pure ColumnConflictAbort)
                        onWord "FAIL" (pure ColumnConflictFail)
                        onWord "IGNORE" (pure ColumnConflictIgnore)
                        onWord "REPLACE" (pure ColumnConflictReplace)
                        lift (unexpectedHere "expected a conflict resolution action")
                    pure (ColumnConstraintOnConflict constraint action)
                else pure constraint
        parseGeneratedBody = flip ColumnConstraintGenerated <$> inParens parseExpr <*> parseGeneratedStorage

parseNameOrLiteralRaw :: P (Located ByteString)
parseNameOrLiteralRaw =
    peekTok >>= \case
        Just (L sp (TokWord wt)) -> advanceTok $> L sp (wordRaw wt)
        Just (L sp (TokLiteral _ raw)) -> advanceTok $> L sp raw
        _ -> unexpectedHere "expected a name or string value"

parseGeneratedStorage :: P Bool
parseGeneratedStorage = evalContT do
    onWord "STORED" (pure True)
    onWord "VIRTUAL" (pure False)
    pure True

parseForeignKeyRef :: P (Located ForeignKeyRef)
parseForeignKeyRef = withLocated (expectWordCI "REFERENCES" *> parseForeignKeyRefAfter)

parseForeignKeyRefAfter :: P ForeignKeyRef
parseForeignKeyRefAfter = do
    table <- parseName
    columns <- parseOptionalColumnNames
    go (ForeignKeyRef table columns Nothing Nothing Nothing Nothing Nothing)
    where
        go ref = evalContT do
            onWord "ON" $ evalContT do
                onWord "DELETE" do
                    action <- parseForeignKeyAction
                    go ref {foreignKeyOnDelete = Just action}
                lift do
                    _ <- expectWordCI "UPDATE"
                    action <- parseForeignKeyAction
                    go ref {foreignKeyOnUpdate = Just action}
            onWord "MATCH" do
                method <- parseNamePart
                go ref {foreignKeyMatch = Just method}
            onWordTok "DEFERRABLE" \tok -> parseDeferrability tok True ref
            nextNot <- lift (wordIsAt 1 "DEFERRABLE")
            when nextNot $
                onWordTok "NOT" \tok -> expectWordCI "DEFERRABLE" *> parseDeferrability tok False ref
            onWordTok "INITIALLY" \tok -> do
                requireDialectP [PostgreSQL, SQLite, DuckDB] (locSpan tok) "foreign key timing"
                timing <- evalContT do
                    onWord "DEFERRED" (pure ForeignKeyInitiallyDeferred)
                    lift (expectWordCI "IMMEDIATE" $> ForeignKeyInitiallyImmediate)
                go ref {foreignKeyInitially = Just timing}
            pure ref

        parseDeferrability tok deferrable ref = do
            requireDialectP [PostgreSQL, SQLite, DuckDB] (locSpan tok) "foreign key deferrability"
            go ref {foreignKeyDeferrable = Just deferrable}

parseForeignKeyAction :: P (Located ForeignKeyAction)
parseForeignKeyAction = withLocated $ evalContT do
    onWordTok "CASCADE" \tok -> rejectDuckDB tok "CASCADE" $> ForeignKeyAction ForeignKeyCascade
    onWord "RESTRICT" (pure (ForeignKeyAction ForeignKeyRestrict))
    onWord "NO" (expectWordCI "ACTION" $> ForeignKeyAction ForeignKeyNoAction)
    onWordTok "SET" \tok -> do
        mNull <- matchWordCI "NULL"
        case mNull of
            Just _ -> rejectDuckDB tok "SET NULL" $> ForeignKeyAction ForeignKeySetNull
            Nothing ->
                expectWordCI "DEFAULT" *> rejectDuckDB tok "SET DEFAULT" $> ForeignKeyAction ForeignKeySetDefault
    lift (unexpectedHere "expected a foreign key action")
    where
        rejectDuckDB tok action = do
            duck <- isDialect DuckDB
            when
                duck
                (unexpectedUnsupported tok ("FOREIGN KEY ... " <> action <> " is not supported by DuckDB"))

parseCreateIndex :: P CreateIndex
parseCreateIndex = do
    noName <- wordIsCurrent "ON"
    when noName $ do
        sp <- currentSpan
        requireDialectP [PostgreSQL] sp "unnamed CREATE INDEX"
    name <- if noName then pure Nothing else Just <$> parseName
    mMethodBefore <- whenWord "USING" parseName
    _ <- expectWordCI "ON"
    table <- parseName
    mMethod <- whenWord "USING" parseName
    keys <- parseIndexKeyList
    includes <-
        fromMaybe [] <$> whenWordTok "INCLUDE" \tok -> do
            requireDialectP [PostgreSQL] (locSpan tok) "CREATE INDEX ... INCLUDE"
            parseParenList parseNamePart
    mMethodAfter <- whenWord "USING" parseName
    whereExpr <- whenWordTok "WHERE" \tok -> do
        requireDialectP [PostgreSQL, SQLite] (locSpan tok) "CREATE INDEX ... WHERE"
        parseExpr
    pure
        CreateIndex
            { createIndexName = name
            , createIndexTable = table
            , createIndexMethod = mMethodBefore <|> mMethod <|> mMethodAfter
            , createIndexKeys = keys
            , createIndexIncludes = includes
            , createIndexWhere = whereExpr
            }

data ViewOptAcc = ViewOptAcc
    { voTo :: !(Maybe (Located Name))
    , voEngine :: !(Maybe OpaqueBody)
    , voSettings :: ![Located SettingItem]
    , voPopulate :: !Bool
    }

parseCreateView :: Bool -> P CreateView
parseCreateView materialized = do
    ifNotExists <- parseIfNotExists
    name <- parseName
    cols <- parseOptionalColumnNames
    ViewOptAcc mTo engine settings populate <-
        execStateT parseViewOptions (ViewOptAcc Nothing Nothing [] False)
    _ <- expectWordCI "AS"
    isQuery <- queryStartsAt 0
    body <-
        if isQuery
            then ViewBodyQuery <$> parseQuery
            else do
                opaque <- parseOpaqueToEnd OpaqueViewBody
                when (spanIsEmpty (opaqueBodySpan opaque)) (unexpectedHere "expected a view query body")
                pure (ViewBodyOpaque opaque)
    withData <- if materialized then parseOptionalWithData else pure Nothing
    pure
        CreateView
            { createViewName = name
            , createViewIfNotExists = ifNotExists
            , createViewColumns = cols
            , createViewMaterialized = materialized
            , createViewTo = mTo
            , createViewEngine = engine
            , createViewSettings = reverse settings
            , createViewPopulate = populate
            , createViewBody = body
            , createViewWithData = withData
            }
    where
        parseViewOptions :: StateT ViewOptAcc P ()
        parseViewOptions =
            lift currentHeadWord >>= \case
                Just w
                    | asciiEqCI "TO" w -> do
                        target <- lift do
                            tok <- expectWordCI "TO"
                            requireDialectP [ClickHouse] (locSpan tok) "CREATE VIEW ... TO"
                            parseName
                        modify' (\s -> s {voTo = Just target})
                        parseViewOptions
                    | asciiEqCI "ENGINE" w -> do
                        body <- lift do
                            tok <- expectWordCI "ENGINE"
                            requireFeatureP FeatureEngineClause (locSpan tok) "ENGINE clause"
                            _ <- matchTok TokEq
                            b <- parseOpaqueUntil OpaqueEngineExpression ["AS", "SETTINGS", "TO", "POPULATE"]
                            when (spanIsEmpty (opaqueBodySpan b)) (unexpectedHere "expected an ENGINE expression")
                            pure b
                        modify' (\s -> s {voEngine = Just body})
                        parseViewOptions
                    | asciiEqCI "SETTINGS" w -> do
                        items <- lift do
                            tok <- expectWordCI "SETTINGS"
                            requireFeatureP FeaturePrewhereSettings (locSpan tok) "SETTINGS"
                            parseCommaSeparated parseSettingItem
                        modify' (\s -> s {voSettings = reverse items ++ voSettings s})
                        parseViewOptions
                    | asciiEqCI "POPULATE" w -> do
                        lift do
                            tok <- expectWordCI "POPULATE"
                            requireDialectP [ClickHouse] (locSpan tok) "CREATE MATERIALIZED VIEW ... POPULATE"
                            unless materialized $
                                throwParse ParseUnsupported (locSpan tok) "POPULATE is only valid for materialized views"
                        modify' (\s -> s {voPopulate = True})
                        parseViewOptions
                _ -> pure ()

parseCreateSequence :: P CreateSequence
parseCreateSequence = do
    ifNotExists <- parseIfNotExists
    name <- parseName
    options <- parseTableOptionList []
    pure (CreateSequence name ifNotExists (reverse options))

parseCreateSchema :: P CreateSchema
parseCreateSchema = do
    ifNotExists <- parseIfNotExists
    nextIsAuth <- wordIsCurrent "AUTHORIZATION"
    name <- if nextIsAuth then pure Nothing else Just <$> parseName
    auth <- whenWord "AUTHORIZATION" parseName
    pure (CreateSchema name ifNotExists auth)

parseCreateDatabase :: P CreateDatabase
parseCreateDatabase = do
    ifNotExists <- parseIfNotExists
    name <- parseName
    options <- parseTableOptionList []
    pure (CreateDatabase name ifNotExists (reverse options))

parseCreateExtension :: P CreateExtension
parseCreateExtension = do
    ifNotExists <- parseIfNotExists
    name <- parseNamePart
    _ <- matchWordCI "WITH"
    skipRemainingTokens
    pure (CreateExtension name ifNotExists)
    where
        skipRemainingTokens =
            peekTok >>= \case
                Nothing -> pure ()
                Just _ -> advanceTok *> skipRemainingTokens

parseCreateStatistics :: P CreateStatistics
parseCreateStatistics = do
    ifNotExists <- parseIfNotExists
    name <- parseName
    kinds <- parseOptParenNonEmptyList parseNamePart
    _ <- expectWordCI "ON"
    columns <- parseCommaSeparated parseExpr
    fromTable <- whenWord "FROM" parseName
    pure (CreateStatistics name ifNotExists kinds columns fromTable)

parseCreateMacro :: P CreateMacro
parseCreateMacro = do
    name <- parseName
    params <- parseOptParenList parseMacroParam
    _ <- expectWordCI "AS"
    isTable <- wordIsCurrent "TABLE"
    body <-
        if isTable
            then Left <$> (expectWordCI "TABLE" *> parseQuery)
            else Right <$> parseExpr
    pure (CreateMacro name params isTable body)

parseMacroParam :: P (Located MacroParam)
parseMacroParam = withLocated (MacroParam <$> parseNamePart <*> whenTok TokColonEq parseExpr)

parseCreatePolicy :: P CreatePolicy
parseCreatePolicy = do
    name <- parseName
    _ <- expectWordCI "ON"
    table <- parseName
    execStateT parseClauses (CreatePolicy name table Nothing Nothing [] Nothing Nothing)
    where
        parseClauses :: StateT CreatePolicy P ()
        parseClauses =
            lift currentHeadWord >>= \case
                Nothing -> pure ()
                Just w
                    | asciiEqCI "AS" w -> do
                        perm <-
                            lift $
                                expectWordCI "AS" *> evalContT do
                                    onWord "PERMISSIVE" (pure True)
                                    lift (expectWordCI "RESTRICTIVE" $> False)
                        modify' (\p -> p {createPolicyPermissive = Just perm})
                        parseClauses
                    | asciiEqCI "FOR" w -> do
                        cmd <- lift do
                            _ <- expectWordCI "FOR"
                            matchAnyWordCI ["ALL", "SELECT", "INSERT", "UPDATE", "DELETE"]
                                >>= maybe
                                    (unexpectedHere "expected a policy command")
                                    (\(L sp _, c) -> pure (L sp c))
                        modify' (\p -> p {createPolicyCommand = Just cmd})
                        parseClauses
                    | asciiEqCI "TO" w -> do
                        roles <- lift (expectWordCI "TO" *> parseCommaSeparated parseName)
                        modify' (\p -> p {createPolicyRoles = roles})
                        parseClauses
                    | asciiEqCI "USING" w -> do
                        expr <- lift (expectWordCI "USING" *> inParens parseExpr)
                        modify' (\p -> p {createPolicyUsing = Just expr})
                        parseClauses
                    | asciiEqCI "WITH" w -> do
                        expr <- lift (expectWordsCI ["WITH", "CHECK"] *> inParens parseExpr)
                        modify' (\p -> p {createPolicyWithCheck = Just expr})
                        parseClauses
                    | otherwise -> lift (unexpectedHere "expected a policy clause")

parseCreateRule :: P CreateRule
parseCreateRule = do
    name <- parseName
    expectWordsCI ["AS", "ON"]
    eventTok <-
        matchAnyWordCI ["SELECT", "INSERT", "UPDATE", "DELETE"]
            >>= maybe
                (unexpectedHere "expected a rule event")
                (\(L sp _, cmd) -> pure (L sp cmd))
    _ <- expectWordCI "TO"
    table <- parseName
    whereExpr <- whenWord "WHERE" parseExpr
    _ <- expectWordCI "DO"
    _ <- matchWordCI "ALSO"
    instead <- isJust <$> matchWordCI "INSTEAD"
    action <- evalContT do
        onWord "NOTHING" (pure Nothing)
        lift do
            mt <- peekTok
            case mt of
                Just (L _ TokLParen) ->
                    unexpectedHere "multi-statement CREATE RULE actions are not supported"
                _ -> Just <$> parseStatementHook
    pure (CreateRule name eventTok table whereExpr instead action)

parseCreateTrigger :: P CreateTrigger
parseCreateTrigger = do
    name <- parseName
    timing <-
        matchAnyWordCI ["BEFORE", "AFTER", "INSTEAD"] >>= traverse \(L sp _, tw) -> do
            when (asciiEqCI "INSTEAD" tw) do
                requireDialectP [PostgreSQL, SQLite] sp "INSTEAD OF trigger"
                void (expectWordCI "OF")
            pure (L sp tw)
    events <- parseTriggerEvents []
    _ <- expectWordCI "ON"
    table <- parseName
    _ <- whenWordTok "REFERENCING" \tok -> do
        requireDialectP [PostgreSQL] (locSpan tok) "CREATE TRIGGER ... REFERENCING"
        parseTriggerReferencing
    granularity <- whenWordTok "FOR" \tok -> do
        pg <- isDialect PostgreSQL
        if pg
            then void (matchWordCI "EACH")
            else void (expectWordCI "EACH")
        mode <- evalContT do
            onWord "ROW" (pure TriggerForEachRow)
            onWordTok "STATEMENT" \statement ->
                requireDialectP [PostgreSQL] (locSpan statement) "FOR EACH STATEMENT" $> TriggerForEachStatement
            lift (unexpectedHere "expected ROW or STATEMENT")
        locatedSpanning (locSpan tok) mode
    mysqlLike <- (||) <$> isDialect MySQL <*> isDialect MariaDB
    when (mysqlLike && isNothing granularity) $
        unexpectedHere "MySQL/MariaDB triggers require FOR EACH ROW"
    whenExpr <- whenWordTok "WHEN" \tok -> do
        requireDialectP [PostgreSQL, SQLite] (locSpan tok) "CREATE TRIGGER ... WHEN"
        pg <- isDialect PostgreSQL
        if pg then inParens parseExpr else parseExpr
    mExecute <- matchWordCI "EXECUTE"
    (executeName, executeArgs, body) <- case mExecute of
        Just exTok -> do
            requireDialectP [PostgreSQL] (locSpan exTok) "CREATE TRIGGER ... EXECUTE"
            evalContT do
                onWord "FUNCTION" (pure ())
                lift (void (expectWordCI "PROCEDURE"))
            fn <- parseName
            args <- parseOptParenList parseExpr
            end <- lastConsumedEnd
            pure (Just fn, args, OpaqueBody (ByteSpan (spanStart (locSpan exTok)) end) OpaqueTriggerBody)
        Nothing -> do
            pg <- isDialect PostgreSQL
            when pg $
                unexpectedHere "PostgreSQL triggers require EXECUTE FUNCTION or EXECUTE PROCEDURE"
            sqlite <- isDialect SQLite
            when sqlite do
                startsBegin <- wordIsCurrent "BEGIN"
                unless startsBegin $
                    unexpectedHere "SQLite triggers require a BEGIN ... END body"
            opaque <- parseOpaqueToEnd OpaqueTriggerBody
            when (spanIsEmpty (opaqueBodySpan opaque)) (unexpectedHere "expected a trigger body")
            pure (Nothing, [], opaque)
    pure
        CreateTrigger
            { createTriggerName = name
            , createTriggerTiming = timing
            , createTriggerEvents = events
            , createTriggerTable = table
            , createTriggerGranularity = granularity
            , createTriggerWhen = whenExpr
            , createTriggerExecute = executeName
            , createTriggerExecuteArgs = executeArgs
            , createTriggerBody = body
            }
    where
        parseTriggerReferencing = do
            _ <-
                matchAnyWordCI ["OLD", "NEW"]
                    >>= maybe (unexpectedHere "expected OLD or NEW after REFERENCING") pure
            _ <- expectWordCI "TABLE"
            _ <- matchWordCI "AS"
            _ <- parseNamePart
            more <- (||) <$> wordIsCurrent "OLD" <*> wordIsCurrent "NEW"
            when more parseTriggerReferencing
        parseTriggerEvents acc =
            matchAnyWordCI ["INSERT", "UPDATE", "DELETE", "TRUNCATE"] >>= \case
                Nothing -> unexpectedHere "expected a trigger event"
                Just (L sp _, eventWord) -> do
                    when (asciiEqCI "TRUNCATE" eventWord) $
                        requireDialectP [PostgreSQL] sp "TRUNCATE trigger event"
                    when (asciiEqCI "UPDATE" eventWord) $
                        void $
                            whenWord "OF" (unexpectedHere "UPDATE OF trigger events are not supported")
                    matchWordCI "OR" >>= \case
                        Just orTok -> do
                            requireDialectP [PostgreSQL] (locSpan orTok) "multiple OR trigger events"
                            parseTriggerEvents (L sp eventWord : acc)
                        Nothing -> pure (reverse (L sp eventWord : acc))

data RoutineAcc = RoutineAcc
    { raReturns :: !(Maybe (Located TypeName))
    , raLanguage :: !(Maybe (Located NamePart))
    , raBody :: !(Maybe OpaqueBody)
    }

parseCreateRoutine :: Bool -> P CreateRoutine
parseCreateRoutine isProcedure = do
    name <- parseName
    ch <- isDialect ClickHouse
    if ch
        then do
            _ <- expectWordCI "AS"
            body <- parseOpaqueToEnd OpaqueFunctionBody
            when
                (spanIsEmpty (opaqueBodySpan body))
                (unexpectedHere "expected a ClickHouse function lambda body")
            pure
                CreateRoutine
                    { createRoutineName = name
                    , createRoutineParams = []
                    , createRoutineReturns = Nothing
                    , createRoutineLanguage = Nothing
                    , createRoutineBody = body
                    }
        else do
            params <- parseOptionalParenList parseRoutineParam
            RoutineAcc returnsType language mBody <-
                execStateT parseClauses (RoutineAcc Nothing Nothing Nothing)
            mysqlLike <- (||) <$> isDialect MySQL <*> isDialect MariaDB
            when (mysqlLike && not isProcedure && isNothing returnsType) $
                unexpectedHere "MySQL/MariaDB CREATE FUNCTION requires RETURNS"
            body <- maybe (unexpectedHere "expected a routine body") pure mBody
            when (spanIsEmpty (opaqueBodySpan body)) (unexpectedHere "expected a routine body")
            let kind = if isProcedure then OpaqueProcedureBody else OpaqueFunctionBody
            pure
                CreateRoutine
                    { createRoutineName = name
                    , createRoutineParams = params
                    , createRoutineReturns = returnsType
                    , createRoutineLanguage = language
                    , createRoutineBody = body {opaqueBodyKind = kind}
                    }
    where
        parseClauses :: StateT RoutineAcc P ()
        parseClauses =
            lift currentHeadWord >>= \case
                Nothing -> pure ()
                Just w
                    | asciiEqCI "RETURNS" w -> do
                        ty <- lift (expectWordCI "RETURNS" *> parseTypeName)
                        modify' (\s -> s {raReturns = Just ty})
                        parseClauses
                    | asciiEqCI "LANGUAGE" w -> do
                        lang <- lift (expectWordCI "LANGUAGE" *> parseNamePart)
                        modify' (\s -> s {raLanguage = Just lang})
                        parseClauses
                    | asciiEqCI "AS" w -> do
                        body' <- lift do
                            asTok <- expectWordCI "AS"
                            requireDialectP [PostgreSQL] (locSpan asTok) "routine AS body"
                            parsePostgreSQLRoutineBody
                        modify' (\s -> s {raBody = Just body'})
                        parseClauses
                    | wordIn ["BEGIN", "DECLARE", "RETURN"] w -> do
                        body <- lift (parseOpaqueToEnd OpaqueFunctionBody)
                        modify' (\s -> s {raBody = Just body})
                    | wordIn ["DETERMINISTIC", "IMMUTABLE", "STABLE", "VOLATILE", "STRICT", "LEAKPROOF", "WINDOW"] w ->
                        lift advanceTok *> parseClauses
                    | asciiEqCI "SECURITY" w ->
                        lift (expectWordCI "SECURITY" *> matchAnyWordCI ["DEFINER", "INVOKER"]) *> parseClauses
                    | asciiEqCI "SQL" w ->
                        lift (expectWordsCI ["SQL", "SECURITY"] *> matchAnyWordCI ["DEFINER", "INVOKER"]) *> parseClauses
                    | wordIn ["COST", "ROWS"] w ->
                        lift (advanceTok *> parseExpr) *> parseClauses
                    | asciiEqCI "PARALLEL" w ->
                        lift (expectWordCI "PARALLEL" *> matchAnyWordCI ["SAFE", "RESTRICTED", "UNSAFE"]) *> parseClauses
                    | asciiEqCI "CALLED" w ->
                        lift (expectWordsCI ["CALLED", "ON", "NULL", "INPUT"]) *> parseClauses
                    | asciiEqCI "COMMENT" w ->
                        lift (expectWordCI "COMMENT" *> parseExpr) *> parseClauses
                    | otherwise -> lift (unexpectedHere "expected a routine clause")

        parsePostgreSQLRoutineBody =
            peekTok >>= \case
                Just (L sp (TokLiteral kind _))
                    | kind /= LitNumber -> do
                        _ <- advanceTok
                        extraEnd <- whenTok TokComma do
                            peekTok >>= \case
                                Just (L sp2 (TokLiteral kind2 _))
                                    | kind2 /= LitNumber -> advanceTok $> sp2
                                _ -> unexpectedHere "expected a link symbol literal after ',' in AS"
                        pure (OpaqueBody (maybe sp (mergeSpan sp) extraEnd) OpaqueFunctionBody)
                _ -> unexpectedHere "expected a string or dollar-quoted routine body after AS"

parseRoutineParam :: P (Located RoutineParam)
parseRoutineParam = withLocated do
    mode <- fmap (\(L sp _, w) -> L sp w) <$> matchAnyWordCI ["IN", "OUT", "INOUT", "VARIADIC"]
    mw0 <- currentWord
    mw1 <- wordAt 1
    (mName, typeName) <- case (mw0, mw1) of
        (Just (_, firstWord), Just (_, secondWord))
            | not (isKnownTypeWord firstWord)
            , not (isMultiwordTypeLead firstWord secondWord) ->
                (,) . Just <$> parseNamePart <*> parseTypeName
        _ -> (Nothing,) <$> parseTypeName
    defaultValue <- evalContT do
        onWord "DEFAULT" (Just <$> parseExpr)
        onTok TokEq (Just <$> parseExpr)
        pure Nothing
    pure
        RoutineParam
            { routineParamMode = fmap locValue mode
            , routineParamName = mName
            , routineParamType = typeName
            , routineParamDefault = defaultValue
            }

isMultiwordTypeLead :: ByteString -> ByteString -> Bool
isMultiwordTypeLead firstWord secondWord =
    (asciiEqCI "DOUBLE" firstWord && asciiEqCI "PRECISION" secondWord)
        || (asciiEqCI "CHARACTER" firstWord && asciiEqCI "VARYING" secondWord)

isKnownTypeWord :: ByteString -> Bool
isKnownTypeWord = (`HashSet.member` knownTypeWords) . asciiUpperBS

knownTypeWords :: HashSet.HashSet ByteString
knownTypeWords =
    HashSet.fromList
        [ "INT"
        , "INTEGER"
        , "BIGINT"
        , "SMALLINT"
        , "TINYINT"
        , "MEDIUMINT"
        , "TEXT"
        , "VARCHAR"
        , "NVARCHAR"
        , "CHAR"
        , "CHARACTER"
        , "BOOLEAN"
        , "BOOL"
        , "NUMERIC"
        , "DECIMAL"
        , "REAL"
        , "DOUBLE"
        , "FLOAT"
        , "DATE"
        , "TIME"
        , "TIMESTAMP"
        , "INTERVAL"
        , "JSON"
        , "JSONB"
        , "UUID"
        , "BYTEA"
        , "BLOB"
        , "BINARY"
        , "VARBINARY"
        , "SERIAL"
        , "BIGSERIAL"
        , "SMALLSERIAL"
        , "ARRAY"
        , "MAP"
        , "TUPLE"
        , "NULLABLE"
        , "LOWCARDINALITY"
        , "STRING"
        , "INT64"
        , "UINT64"
        , "FLOAT64"
        , "DATETIME"
        , "DATETIME64"
        , "DECIMAL32"
        , "DECIMAL64"
        , "DECIMAL128"
        , "ENUM8"
        , "ENUM16"
        , "FIXEDSTRING"
        , "IPV4"
        , "IPV6"
        , "VARIANT"
        , "OBJECT"
        , "GEOMETRY"
        ]

parseOptionalWithData :: P (Maybe Bool)
parseOptionalWithData =
    whenWordTok "WITH" \tok -> do
        requireDialectP [PostgreSQL] (locSpan tok) "WITH [NO] DATA"
        noData <- isJust <$> matchWordCI "NO"
        _ <- expectWordCI "DATA"
        pure (not noData)

parseCreateAggregate :: P CreateAggregate
parseCreateAggregate = do
    name <- parseName
    _ <- expectTok TokLParen
    (star, params) <-
        peekToken >>= \case
            Just TokStar -> advanceTok $> (True, [])
            _ -> (False,) <$> parseOptionalCommaList parseRoutineParam
    _ <- expectTok TokRParen
    options <- parseParenList parseTableOption
    pure
        CreateAggregate
            { createAggregateName = name
            , createAggregateParams = params
            , createAggregateStar = star
            , createAggregateOptions = options
            }

parseCreateVirtualTable :: P CreateVirtualTable
parseCreateVirtualTable = do
    ifNotExists <- parseIfNotExists
    name <- parseName
    _ <- expectWordCI "USING"
    moduleName <- parseName
    args <- parseOptParenList parseVirtualTableArg
    pure (CreateVirtualTable name ifNotExists moduleName args)

parseVirtualTableArg :: P (Located VirtualTableArg)
parseVirtualTableArg = withLocated do
    mw0 <- currentWord
    mw1 <- wordAt 1
    case (mw0, mw1) of
        (Just _, Just _) -> VirtualTableColumn <$> parseColumnDef
        _ -> VirtualTableExpression <$> parseExpr

parseTableOptionList :: [Located TableOption] -> P [Located TableOption]
parseTableOptionList acc =
    peekTok >>= \case
        Nothing -> pure acc
        Just (L _ TokComma) -> advanceTok *> parseTableOptionList acc
        Just (L _ (TokWord _)) -> do
            option <- parseTableOption
            parseTableOptionList (option : acc)
        _ -> pure acc

-- | Parse one @NAME [= value]@ table, sequence, or database option.
parseTableOption :: P (Located TableOption)
parseTableOption = withLocated do
    start <- currentSpan
    nameParts' <- parseTableOptionName
    validateTableOptionName start nameParts'
    value <- evalContT do
        onTok TokEq (Just <$> parseExpr)
        onTok TokLParen do
            parenStart <- currentSpan
            items <- parseOptionalCommaList parseExpr
            _ <- expectTok TokRParen
            Just <$> locatedSpanning parenStart (ExprTuple items)
        lift do
            let isNegated = case nameParts' of
                    (L _ (NamePart firstRaw _) : _) -> asciiEqCI "NO" firstRaw
                    [] -> False
                lastRaw = maybe "" (namePartRaw . locValue) (lastMaybe nameParts')
            if not isNegated && wordIn optionValueWords lastRaw
                then
                    peekTok >>= \case
                        Just (L _ (TokLiteral _ _)) -> Just <$> parseExpr
                        Just (L _ (TokWord _)) -> Just <$> parseExpr
                        Just (L _ TokMinus) -> Just <$> parseExpr
                        Just (L _ TokPlus) -> Just <$> parseExpr
                        _ -> pure Nothing
                else pure Nothing
    pure (TableOption nameParts' value)
    where
        optionValueWords =
            [ "ENGINE"
            , "CHARSET"
            , "COLLATE"
            , "AUTO_INCREMENT"
            , "COMMENT"
            , "ROW_FORMAT"
            , "KEY_BLOCK_SIZE"
            , "TABLESPACE"
            , "COMPRESSION"
            , "START"
            , "INCREMENT"
            , "MINVALUE"
            , "MAXVALUE"
            , "RESTART"
            , "CACHE"
            , "OWNED"
            , "DEFAULT"
            , "WITH"
            , "BY"
            , "SET"
            , "AS"
            ]

validateTableOptionName :: ByteSpan -> [Located NamePart] -> P ()
validateTableOptionName sp parts
    | anyWord "ENGINE" = requireFeatureP FeatureEngineClause sp "ENGINE table option"
    | anyWord "CHARSET" || (anyWord "CHARACTER" && anyWord "SET") =
        requireDialectP [MySQL, MariaDB] sp "CHARACTER SET table option"
    | anyWord "AUTO_INCREMENT" = requireDialectP [MySQL, MariaDB] sp "AUTO_INCREMENT table option"
    | anyWord "SECONDARY_ENGINE" || anyWord "AUTOEXTEND_SIZE" =
        requireDialectP [MySQL] sp "MySQL-only table option"
    | anyWord "ROW_FORMAT" || anyWord "KEY_BLOCK_SIZE" =
        requireDialectP [MySQL, MariaDB] sp "MySQL table option"
    | anyWord "ORDER" || firstWord "BY" || anyWord "SAMPLE" || anyWord "TTL" =
        requireDialectP [ClickHouse] sp "ClickHouse table clause"
    | anyWord "STRICT" = requireDialectP [SQLite] sp "STRICT table option"
    | anyWord "ROWID" = requireDialectP [SQLite] sp "SQLite table option"
    | anyWord "INHERITS" = requireDialectP [PostgreSQL] sp "INHERITS table option"
    | otherwise = pure ()
    where
        raws = map (namePartRaw . locValue) parts
        anyWord = wordIn raws
        firstWord target = case raws of
            (first : _) -> asciiEqCI target first
            [] -> False

parseTableOptionName :: P [Located NamePart]
parseTableOptionName = do
    first <- parseNamePart
    let firstRaw = namePartRaw (locValue first)
    rest <-
        if asciiEqCI "DEFAULT" firstRaw
            then
                matchAnyWordCI ["CHARSET", "CHARACTER"] >>= \case
                    Just (tok, hit)
                        | asciiEqCI "CHARACTER" hit -> (toPart tok :) <$> optMatchedWord "SET"
                        | otherwise -> pure [toPart tok]
                    Nothing -> pure []
            else
                if asciiEqCI "NO" firstRaw
                    then currentWord >>= maybe (pure []) (const ((: []) <$> parseNamePart))
                    else case find (\(k, _) -> asciiEqCI k firstRaw) twoWordOptions of
                        Just (_, second) -> optMatchedWord second
                        Nothing -> pure []
    pure (first : rest)
    where
        toPart tok = L (locSpan tok) (NamePart (tokenRaw tok) False)
        optMatchedWord w = maybe [] (\tok -> [toPart tok]) <$> matchWordCI w
        twoWordOptions =
            [ ("CHARACTER", "SET")
            , ("WITHOUT", "ROWID")
            , ("START", "WITH")
            , ("INCREMENT", "BY")
            , ("OWNED", "BY")
            ]

-- ---------------------------------------------------------------------------
-- DROP
-- ---------------------------------------------------------------------------

-- | Parse a DROP statement with IF EXISTS, name list, and behavior.
parseDrop :: P (Located DropStmt)
parseDrop = withLocated do
    _ <- expectWordCI "DROP"
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
    ifExists <- parseIfExists
    names <- parseCommaSeparated parseName
    onTable <- whenWordTok "ON" \tok -> do
        case dropObj of
            DropIndex ->
                requireDialectP [MySQL, MariaDB] (locSpan tok) "DROP INDEX ... ON table"
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
    let isIndexDrop = case dropObj of
            DropIndex -> True
            _ -> False
        requiresOn = case dropObj of
            DropTrigger -> True
            DropPolicy -> True
            DropRule -> True
            _ -> False
    when
        (mysqlLike && isIndexDrop && isNothing onTable)
        (throwParse ParseUnsupported objSpan "DROP INDEX requires ON table in MySQL/MariaDB")
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
    pure (DropStmt (L objSpan dropObj) ifExists names onTable behavior syncFlag)

parseDropObjectWord :: ByteString -> SpannedToken -> P DropObject
parseDropObjectWord upper objTok =
    let sp = locSpan objTok
        step p = advanceTok *> p
     in case upper of
            "TABLE" -> step (pure DropTable)
            "INDEX" ->
                step
                    ( requireDialectP [PostgreSQL, MySQL, MariaDB, SQLite, DuckDB] sp "DROP INDEX"
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
    actions <- case objectKind of
        AlterObjectTable -> parseCommaSeparated parseAlterAction
        AlterObjectView -> (: []) <$> parseAlterViewAction
    pure (AlterStmt objectKind ifExists only table actions)

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
            col <- parseColumnDefWith mysqlLike
            pos <- if mysqlLike then parseOptionalColumnPosition else pure Nothing
            pure (AlterModifyColumn [col] pos)
    onWordTok "CHANGE" \tok -> do
        requireDialectP [MySQL, MariaDB] (locSpan tok) "ALTER TABLE ... CHANGE"
        _ <- matchWordCI "COLUMN"
        oldName <- parseNamePart
        col <- parseColumnDefWith True
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
        AlterInherit <$> parseName
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
            AlterNoInherit <$> parseName
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
            col <- parseColumnDefWith mysqlLike
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
