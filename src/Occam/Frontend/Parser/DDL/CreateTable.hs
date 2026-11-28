-- | CREATE TABLE, column, constraint, and table-option grammar.
module Occam.Frontend.Parser.DDL.CreateTable (
    ColumnDefOpts (..),
    parseCreateTable,
    parseIfNotExists,
    parseColumnDef,
    parseColumnDefWith,
    parseTableConstraint,
    parseTableOption,
    parseTableOptionList,
    parseOptionalWithData,
    parsePartitionBoundSpec,
    validatePgRelationName,
) where

import Control.Applicative ((<|>))
import Control.Monad (join, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.State.Strict (StateT, execStateT, modify')
import Data.ByteString (ByteString)
import Data.Foldable (find, traverse_)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Occam.Frontend.AST.DDL
import Occam.Frontend.AST.Expr
import Occam.Frontend.AST.Query
import Occam.Frontend.Lexer hiding (LiteralKind)
import Occam.Frontend.Parser.Core
import Occam.Frontend.Parser.Expr
import Occam.Frontend.Parser.Query
import Occam.Frontend.Span (ByteSpan (..), Located (..), spanIsEmpty)
import Occam.Parser.Dialect (DialectFeature (..))
import Occam.Parser.Types (SQLDialect (..))

-- | Consume @IF NOT EXISTS@ and report whether it was present.
parseIfNotExists :: P Bool
parseIfNotExists = isJust <$> whenWord "IF" (expectWordsCI ["NOT", "EXISTS"])

data CreateTableTail = CreateTableTail
    { cttOptions :: ![Located TableOption]
    , cttEngine :: !(Maybe OpaqueBody)
    , cttSettings :: ![Located SettingItem]
    , cttPartitionBy :: !(Maybe (Located PartitionBy))
    , cttAs :: !(Maybe (Located Query))
    , cttWithData :: !(Maybe Bool)
    }

-- | Parse a CREATE TABLE body, including partition, option, and CTAS tails.
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
    validatePgRelationName parent
    bound <- evalContT do
        onWord "DEFAULT" (pure PartitionBoundDefault)
        lift do
            _ <- expectWordsCI ["FOR", "VALUES"]
            parsePartitionBoundSpec
    locatedSpanning startSp (TablePartition parent bound)

-- | Parse PostgreSQL list, range, hash, or DEFAULT partition-bound syntax.
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

{- | Context controls for optional clauses in a column definition.
'cdoAllowPosition' permits column placement syntax; 'cdoAllowReferences'
permits inline @REFERENCES@ clauses.
-}
data ColumnDefOpts = ColumnDefOpts
    { cdoAllowPosition :: !Bool
    , cdoAllowReferences :: !Bool
    }

-- | Parse one column definition in the default CREATE TABLE context.
parseColumnDef :: P (Located ColumnDef)
parseColumnDef = parseColumnDefWith (ColumnDefOpts False True)

-- | Parse a column definition with caller-selected clause permissions.
parseColumnDefWith :: ColumnDefOpts -> P (Located ColumnDef)
parseColumnDefWith opts = withLocated do
    namePart <- parseNamePart
    mt <- peekTok
    mType <- if tokenStartsType mt then Just <$> parseTypeName else pure Nothing
    constraints <- parseColumnConstraints opts []
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

parseColumnConstraints ::
    ColumnDefOpts -> [Located ColumnConstraint] -> P [Located ColumnConstraint]
parseColumnConstraints opts acc =
    peekTok >>= \case
        Nothing -> pure (reverse acc)
        Just (L _ TokComma) -> pure (reverse acc)
        Just (L _ TokRParen) -> pure (reverse acc)
        Just (L _ (TokWord wt))
            | cdoAllowPosition opts
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
                        constraint <- parseColumnConstraint (cdoAllowReferences opts)
                        parseColumnConstraints opts (constraint : acc)
                    else unexpectedHere "expected a column constraint, ',' or ')'"
            | Just uw <- unquotedWord wt
            , wordIn (columnConstraintStarters ++ ["CHARACTER", "BINARY", "KEY"]) (wordRaw uw) -> do
                constraint <- parseColumnConstraint (cdoAllowReferences opts)
                parseColumnConstraints opts (constraint : acc)
        _ -> unexpectedHere "expected a column constraint, ',' or ')'"

parseColumnConstraint :: Bool -> P (Located ColumnConstraint)
parseColumnConstraint allowReferences = do
    start <- currentSpan
    locatedSpanning start =<< evalContT do
        onWordTok "CONSTRAINT" \tok -> do
            requireNonClickHouseColumnConstraint tok "named column constraint"
            namePart <- parseNamePart
            ColumnConstraintNamed namePart . locValue <$> parseColumnConstraint allowReferences
        onWord "NOT" do
            matchWordCI "NULL"
                >>= maybe
                    (throwParse ParseUnsupported start "only NOT NULL column constraints are supported")
                    (const (withConflict ColumnConstraintNotNull))
        onWord "NULL" (withConflict ColumnConstraintNull)
        onWordTok "DEFAULT" \tok -> do
            expr <- parseExpr
            mysql <- isDialect MySQL
            when (mysql && not (isValidMySQLDefaultExpr (locValue expr))) $
                throwParse
                    ParseUnsupported
                    (locSpan tok)
                    "MySQL DEFAULT requires a literal, CURRENT_TIMESTAMP, or parenthesized expression"
            pure (ColumnConstraintDefault expr)
        onWord "PRIMARY" do
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
            unless allowReferences $
                throwParse
                    ParseUnsupported
                    (locSpan tok)
                    "ALTER TABLE MODIFY/CHANGE COLUMN does not support inline REFERENCES"
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
        isValidMySQLDefaultExpr = \case
            ExprLiteral _ -> True
            ExprNull -> True
            ExprBoolean _ -> True
            ExprParen _ -> True
            ExprTypedString {} -> True
            ExprCharsetString {} -> True
            ExprUnary op (L _ inner)
                | op `elem` [UnaryPositive, UnaryNegate] ->
                    case inner of
                        ExprLiteral _ -> True
                        _ -> False
            ExprColumn (L _ (Name [L _ (NamePart raw False)])) ->
                wordIn
                    ["CURRENT_TIMESTAMP", "NOW", "LOCALTIME", "LOCALTIMESTAMP", "UTC_TIMESTAMP", "UTC_DATE", "UTC_TIME"]
                    raw
            ExprCall (L _ fn) -> case locValue (functionName fn) of
                Name [L _ (NamePart raw False)]
                    | wordIn
                        ["CURRENT_TIMESTAMP", "NOW", "LOCALTIME", "LOCALTIMESTAMP", "UTC_TIMESTAMP", "UTC_DATE", "UTC_TIME"]
                        raw ->
                        null (functionWithinGroup fn)
                            && isNothing (functionFilter fn)
                            && isNothing (functionOver fn)
                            && length (functionArgs fn) <= 1
                _ -> False
            _ -> False

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

-- | Parse PostgreSQL @WITH [NO] DATA@, returning 'Nothing' when absent.
parseOptionalWithData :: P (Maybe Bool)
parseOptionalWithData =
    whenWordTok "WITH" \tok -> do
        requireDialectP [PostgreSQL] (locSpan tok) "WITH [NO] DATA"
        noData <- isJust <$> matchWordCI "NO"
        _ <- expectWordCI "DATA"
        pure (not noData)

-- | Parse further table options, extending the reverse-order accumulator.
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

-- | Reject PostgreSQL relation names with more than three identifier parts.
validatePgRelationName :: Located Name -> P ()
validatePgRelationName (L sp (Name parts)) = do
    pg <- isDialect PostgreSQL
    when (pg && length parts > 3) $
        throwParse ParseUnsupported sp "improper qualified name (too many dotted names)"
