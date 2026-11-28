-- | Utility and administrative statement parsers (EXPLAIN, COPY, ANALYZE, VACUUM, ATTACH, PREPARE, TRUNCATE, PIVOT, SHOW).
module Occam.Frontend.Parser.Stmt.Utility (
    parseRefreshMaterializedView,
    parseExplain,
    parseCopy,
    parseAnalyze,
    parseVacuum,
    parseAttach,
    parseDetach,
    parsePrepare,
    parseExecute,
    parseDeallocate,
    parseTruncate,
    parsePivot,
    parseNotify,
    parseRenameTable,
    parseOptimize,
    parseShow,
) where

import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust)
import Occam.Frontend.AST.Expr
import Occam.Frontend.AST.Statement
import Occam.Frontend.Lexer hiding (LiteralKind)
import Occam.Frontend.Parser.Core
import Occam.Frontend.Parser.DDL (parseDropBehavior, parseIfExists, parseIfNotExists)
import Occam.Frontend.Parser.Expr
import Occam.Frontend.Parser.Query (parseQuery, parseSelectItem, parseTableRef)
import Occam.Frontend.Span (ByteSpan (..), Located (..), spanEnd, spanStart)
import Occam.Parser.Dialect (DialectFeature (..))
import Occam.Parser.Types (SQLDialect (..))

-- | Parse PostgreSQL @REFRESH MATERIALIZED VIEW@ and its concurrency/data flags.
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

-- | Parse @EXPLAIN@ options followed by the statement being explained.
parseExplain :: P (Located ExplainStmt)
parseExplain = withLocated do
    _ <- expectWordCI "EXPLAIN"
    options <- parseExplainOptions []
    _ <- currentHeadWord >>= maybe (unexpectedHere "expected a statement after EXPLAIN") pure
    ExplainStmt (reverse options) <$> parseStatementHook

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

-- | Parse @COPY@ source, direction, endpoint, and supported option spellings.
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

-- | Parse @ANALYZE@ with an optional table and column list.
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

-- | Parse SQLite @VACUUM INTO@ or PostgreSQL/DuckDB vacuum options and target.
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

-- | Parse database detachment or ClickHouse object detachment.
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

-- | Parse a prepared statement using the dialect-specific source form.
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
            PrepareStmt name params . PrepareStatement <$> (expectWordCI "AS" *> parseStatementHook)
    where
        isMySQLPrepareSource = \case
            ExprLiteral (Literal LitNumber _) -> False
            ExprLiteral _ -> True
            ExprCharsetString {} -> True
            ExprParam ParamUser _ -> True
            _ -> False

-- | Parse execution of a prepared statement with optional expression arguments.
parseExecute :: P (Located ExecuteStmt)
parseExecute = withLocated do
    tok <- expectWordCI "EXECUTE"
    requireFeatureP FeaturePreparedStatements (locSpan tok) "EXECUTE"
    ExecuteStmt <$> parseName <*> parseOptParenList parseExpr

-- | Parse @DEALLOCATE ALL@ or deallocation of one prepared name.
parseDeallocate :: P (Located DeallocateStmt)
parseDeallocate = withLocated do
    tok <- expectWordCI "DEALLOCATE"
    requireFeatureP FeaturePreparedStatements (locSpan tok) "DEALLOCATE"
    _ <- matchWordCI "PREPARE"
    evalContT do
        onWord "ALL" (pure (DeallocateStmt True Nothing))
        lift (DeallocateStmt False . Just <$> parseName)

-- | Parse @TRUNCATE@ targets and supported identity/cascade modifiers.
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

-- | Parse DuckDB @PIVOT@ or @UNPIVOT@ into its structural clause fields.
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

-- | Parse PostgreSQL @NOTIFY@ channel and optional payload.
parseNotify :: P (Located NotifyStmt)
parseNotify = withLocated do
    tok <- expectWordCI "NOTIFY"
    requireDialectP [PostgreSQL] (locSpan tok) "NOTIFY"
    channel <- parseName
    payload <- whenTok TokComma parseExpr
    pure (NotifyStmt channel payload)

-- | Parse the MySQL/MariaDB/ClickHouse pairwise @RENAME TABLE@ form.
parseRenameTable :: P (Located [(Located Name, Located Name)])
parseRenameTable = withLocated do
    tok <- expectWordCI "RENAME"
    _ <- expectWordCI "TABLE"
    requireDialectP [MySQL, MariaDB, ClickHouse] (locSpan tok) "RENAME TABLE"
    parseCommaSeparated ((,) <$> parseName <*> (expectWordCI "TO" *> parseName))

-- | Parse ClickHouse/MySQL @OPTIMIZE TABLE@ modifiers.
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

-- | Parse a dialect-specific @SHOW@ subject, target, and filters.
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
