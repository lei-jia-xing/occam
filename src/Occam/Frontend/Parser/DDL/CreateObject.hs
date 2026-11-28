-- | CREATE dispatch and schema-object parsers.
module Occam.Frontend.Parser.DDL.CreateObject (
    parseCreate,
) where

import Control.Applicative ((<|>))
import Control.Monad (unless, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.State.Strict (StateT, execStateT, modify')
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.HashSet qualified as HashSet
import Data.Maybe (fromMaybe, isJust, isNothing)
import Occam.Frontend.AST.DDL
import Occam.Frontend.AST.Expr
import Occam.Frontend.AST.Query
import Occam.Frontend.Lexer hiding (LiteralKind)
import Occam.Frontend.Parser.Core
import Occam.Frontend.Parser.DDL.CreateTable (
    parseColumnDef,
    parseCreateTable,
    parseIfNotExists,
    parseOptionalWithData,
    parseTableOption,
    parseTableOptionList,
 )
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
    , cfUnlogged :: !(Maybe ByteSpan)
    , cfUnique :: !(Maybe ByteSpan)
    , cfFulltext :: !(Maybe ByteSpan)
    , cfMaterialized :: !(Maybe ByteSpan)
    , cfVirtual :: !(Maybe ByteSpan)
    , cfAlgorithm :: !(Maybe (Located ByteString))
    }

emptyCreateFlags :: CreateFlags
emptyCreateFlags = CreateFlags Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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
            "INDEX" -> build (CreateIndexObject <$> parseCreateIndex)
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
                    requireDialectP
                        [PostgreSQL, SQLite, DuckDB]
                        (locSpan tok)
                        "TEMP spelling (use TEMPORARY in MySQL/MariaDB/ClickHouse)"
                parseCreateFlags flags {cfTemp = Just (locSpan tok)}
            onWordTok "UNLOGGED" \tok ->
                parseCreateFlags flags {cfUnlogged = Just (locSpan tok)}
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
    traverse_ checkUnlogged (cfUnlogged flags)
    traverse_ checkMat (cfMaterialized flags)
    traverse_ checkUnique (cfUnique flags)
    traverse_ checkFulltext (cfFulltext flags)
    traverse_ checkVirtual (cfVirtual flags)
    traverse_ checkAlgorithm (cfAlgorithm flags)
    where
        checkOrReplace sp = case objRaw of
            "TABLE" -> requireDialectP [DuckDB, ClickHouse, MariaDB] sp "CREATE OR REPLACE TABLE"
            "INDEX" -> requireDialectP [MariaDB] sp "CREATE OR REPLACE INDEX"
            "VIEW"
                | isJust (cfMaterialized flags) ->
                    requireDialectP [ClickHouse] sp "CREATE OR REPLACE MATERIALIZED VIEW"
                | otherwise ->
                    requireDialectP [PostgreSQL, MySQL, MariaDB, DuckDB, ClickHouse] sp "CREATE OR REPLACE VIEW"
            "FUNCTION" -> requireDialectP [PostgreSQL, MariaDB, ClickHouse] sp "CREATE OR REPLACE FUNCTION"
            "PROCEDURE" -> requireDialectP [PostgreSQL, MariaDB] sp "CREATE OR REPLACE PROCEDURE"
            "AGGREGATE" -> requireDialectP [PostgreSQL] sp "CREATE OR REPLACE AGGREGATE"
            "TRIGGER" -> requireDialectP [PostgreSQL, MariaDB] sp "CREATE OR REPLACE TRIGGER"
            "SEQUENCE" -> requireDialectP [MariaDB, DuckDB] sp "CREATE OR REPLACE SEQUENCE"
            "DATABASE" -> requireDialectP [MariaDB] sp "CREATE OR REPLACE DATABASE"
            "SCHEMA" -> requireDialectP [DuckDB, MariaDB] sp "CREATE OR REPLACE SCHEMA"
            "MACRO" -> requireDialectP [DuckDB] sp "CREATE OR REPLACE MACRO"
            "RULE" -> requireDialectP [PostgreSQL] sp "CREATE OR REPLACE RULE"
            _ -> throwParse ParseUnsupported sp ("CREATE OR REPLACE " <> decodeBS objRaw <> " is not supported")
        checkTemp sp = case objRaw of
            "TABLE" -> pure ()
            "VIEW"
                | isJust (cfMaterialized flags) ->
                    throwParse ParseUnsupported sp "CREATE TEMPORARY MATERIALIZED VIEW is not supported"
                | otherwise ->
                    requireDialectP [PostgreSQL, SQLite, DuckDB, ClickHouse] sp "TEMP VIEW"
            "SEQUENCE" -> requireDialectP [PostgreSQL, DuckDB, MariaDB] sp "TEMP SEQUENCE"
            "MACRO" -> requireDialectP [DuckDB] sp "TEMP MACRO"
            "POLICY" -> throwParse ParseUnsupported sp "CREATE TEMP POLICY is not valid in PostgreSQL"
            _ -> throwParse ParseUnsupported sp ("CREATE TEMP " <> decodeBS objRaw <> " is not supported")
        checkUnlogged sp = case objRaw of
            "TABLE" -> requireDialectP [PostgreSQL] sp "CREATE UNLOGGED TABLE"
            "SEQUENCE" -> requireDialectP [PostgreSQL] sp "CREATE UNLOGGED SEQUENCE"
            _ -> throwParse ParseUnsupported sp ("CREATE UNLOGGED " <> decodeBS objRaw <> " is not supported")
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

parseCreateIndex :: P CreateIndex
parseCreateIndex = do
    noName <- wordIsCurrent "ON"
    when noName $ do
        sp <- currentSpan
        requireDialectP [PostgreSQL] sp "unnamed CREATE INDEX"
    name <- if noName then pure Nothing else Just <$> parseName
    let parseIndexUsing =
            whenWordTok "USING" \tok ->
                requireDialectP [PostgreSQL, MySQL, MariaDB, DuckDB] (locSpan tok) "CREATE INDEX ... USING"
                    *> parseName
    mMethodBefore <- parseIndexUsing
    _ <- expectWordCI "ON"
    table <- parseName
    mMethod <- parseIndexUsing
    keys <- parseIndexKeyList
    includes <-
        fromMaybe [] <$> whenWordTok "INCLUDE" \tok -> do
            requireDialectP [PostgreSQL] (locSpan tok) "CREATE INDEX ... INCLUDE"
            parseParenList parseNamePart
    mMethodAfter <- parseIndexUsing
    whereExpr <- whenWordTok "WHERE" \tok -> do
        requireDialectP [PostgreSQL, SQLite] (locSpan tok) "CREATE INDEX ... WHERE"
        parseExpr
    ch <- isDialect ClickHouse
    when ch $
        void $
            whenWord "TYPE" (parseTypeName *> whenWord "GRANULARITY" parseExpr)
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
    mysqlLike <- (||) <$> isDialect MySQL <*> isDialect MariaDB
    when (mysqlLike && hasUnquotedReservedRoutineName (locValue name)) $
        throwParse ParseUnexpectedToken (locSpan name) "reserved keyword cannot be used as routine name"
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
        hasUnquotedReservedRoutineName (Name parts) =
            any
                ( \(L _ (NamePart raw quoted)) ->
                    not quoted && wordIn mysqlReservedRoutineNames raw
                )
                parts
        mysqlReservedRoutineNames =
            [ "VALUES"
            , "SELECT"
            , "FROM"
            , "WHERE"
            , "INSERT"
            , "UPDATE"
            , "DELETE"
            , "TABLE"
            , "ORDER"
            , "GROUP"
            , "HAVING"
            , "JOIN"
            , "UNION"
            , "EXCEPT"
            , "INTERSECT"
            , "CREATE"
            , "DROP"
            , "ALTER"
            , "SET"
            , "INTO"
            , "AS"
            , "ON"
            , "AND"
            , "OR"
            , "NOT"
            , "NULL"
            , "TRUE"
            , "FALSE"
            , "DEFAULT"
            , "CASE"
            , "WHEN"
            , "THEN"
            , "ELSE"
            , "END"
            , "RETURN"
            ]
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
