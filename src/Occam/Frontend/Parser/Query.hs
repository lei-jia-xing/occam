-- | Query, CTE, SELECT, FROM/JOIN, and query-tail parsers.
module Occam.Frontend.Parser.Query (
    parseQuery,
    parseQueryRest,
    parseOptionalWith,
    parseFromClause,
    parseTableRef,
    parseValuesRows,
    parseSelectItem,
    parseLimitClause,
) where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.State.Strict (StateT, execStateT, gets, modify')
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Occam.Frontend.AST
import Occam.Frontend.Lexer hiding (LiteralKind)
import Occam.Frontend.Parser.Core
import Occam.Frontend.Parser.Expr
import Occam.Frontend.Span (ByteSpan, Located (..), mergeSpan)
import Occam.Parser.Dialect (DialectFeature (..), DialectProfile (..))
import Occam.Parser.Types (SQLDialect (..))

-- ---------------------------------------------------------------------------
-- WITH and queries
-- ---------------------------------------------------------------------------

-- | Parse an optional @WITH [RECURSIVE]@ clause.
parseOptionalWith :: P (Maybe (Located WithClause))
parseOptionalWith = whenWordTok "WITH" \tok -> do
    recursive <- isJust <$> matchWordCI "RECURSIVE"
    ctes <- parseCommaSeparated parseCte
    locatedSpanning (locSpan tok) (WithClause recursive ctes)

parseCte :: P (Located Cte)
parseCte = withLocated do
    namePart <- parseNamePart
    cols <- parseOptionalColumnNames
    _ <- expectWordCI "AS"
    materialized <- parseMaterializedHint
    body <- inParens $ inSubqueryP $ withLocated do
        mHead <- currentHeadWord
        if maybe False (wordIn ["INSERT", "UPDATE", "DELETE", "MERGE"]) mHead
            then CteStatement <$> parseStatementHook
            else
                if maybe False (asciiEqCI "WITH") mHead
                    then do
                        stmt <- parseStatementHook
                        case locValue stmt of
                            StatementQuery q -> pure (CteQuery q)
                            _ -> pure (CteStatement stmt)
                    else CteQuery <$> parseQuery
    pure (Cte namePart cols materialized body)

parseMaterializedHint :: P (Maybe MaterializedHint)
parseMaterializedHint = evalContT do
    onWord "MATERIALIZED" (pure (Just Materialized))
    onWord "NOT" (expectWordCI "MATERIALIZED" $> Just NotMaterialized)
    pure Nothing

-- | Parse a complete query, including an optional WITH clause.
parseQuery :: P (Located Query)
parseQuery = do
    start <- currentSpan
    mWith <- parseOptionalWith
    parseQueryRest start mWith

-- | Parse a query body and tail after an already-parsed optional WITH clause.
parseQueryRest :: ByteSpan -> Maybe (Located WithClause) -> P (Located Query)
parseQueryRest start mWith = do
    body <- parseQueryBody
    tailInfo <- parseQueryTail
    bodyWithInto <- case queryTailInto tailInfo of
        Nothing -> pure body
        Just into -> case locValue body of
            QueryBodySelect core
                | isJust (selectInto core) ->
                    throwParse ParseUnexpectedToken (locSpan into) "duplicate SELECT INTO clause"
                | otherwise -> locatedSpanning (locSpan body) (QueryBodySelect core {selectInto = Just into})
            _ -> throwParse ParseUnsupported (locSpan into) "trailing INTO requires a SELECT query"
    locatedSpanning
        start
        Query
            { queryWith = mWith
            , queryBody = bodyWithInto
            , queryOrderBy = queryTailOrder tailInfo
            , queryLimit = queryTailLimit tailInfo
            , queryOffset = queryTailOffset tailInfo
            , queryFetch = queryTailFetch tailInfo
            , queryFor = queryTailFor tailInfo
            , querySettings = queryTailSettings tailInfo
            , queryFormat = queryTailFormat tailInfo
            }

data QueryTailInfo = QueryTailInfo
    { queryTailOrder :: ![Located OrderItem]
    , queryTailLimit :: !(Maybe (Located LimitClause))
    , queryTailOffset :: !(Maybe (Located Expr))
    , queryTailFetch :: !(Maybe (Located FetchClause))
    , queryTailFor :: !(Maybe (Located ForClause))
    , queryTailSettings :: !(Maybe (Located [Located SettingItem]))
    , queryTailFormat :: !(Maybe (Located NamePart))
    , queryTailInto :: !(Maybe (Located SelectInto))
    }

parseQueryTail :: P QueryTailInfo
parseQueryTail = execStateT go (QueryTailInfo [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
    where
        go :: StateT QueryTailInfo P ()
        go =
            lift currentHeadWord >>= \case
                Just w
                    | asciiEqCI "ORDER" w ->
                        setUniqueList
                            queryTailOrder
                            "ORDER BY"
                            (expectWordCI "BY" *> parseCommaSeparated parseOrderItem)
                            (\items acc -> acc {queryTailOrder = items})
                    | asciiEqCI "LIMIT" w ->
                        setUniqueMaybe queryTailLimit "LIMIT" parseLimitClause (\c acc -> acc {queryTailLimit = Just c})
                    | asciiEqCI "OFFSET" w ->
                        setUniqueMaybe
                            queryTailOffset
                            "OFFSET"
                            (parseExpr <* matchAnyWordCI ["ROW", "ROWS"])
                            (\e acc -> acc {queryTailOffset = Just e})
                    | asciiEqCI "FETCH" w ->
                        setUniqueMaybe queryTailFetch "FETCH" parseFetchClause (\c acc -> acc {queryTailFetch = Just c})
                    | asciiEqCI "SETTINGS" w ->
                        setUniqueMaybeWithTok
                            queryTailSettings
                            "SETTINGS"
                            ( \tok ->
                                requireFeatureP FeaturePrewhereSettings (locSpan tok) "SETTINGS"
                                    *> (parseCommaSeparated parseSettingItem >>= locatedSpanning (locSpan tok))
                            )
                            (\items acc -> acc {queryTailSettings = Just items})
                    | asciiEqCI "FORMAT" w ->
                        setUniqueMaybeWithTok
                            queryTailFormat
                            "FORMAT"
                            (\tok -> requireDialectP [ClickHouse] (locSpan tok) "FORMAT clause" *> parseNamePart)
                            (\fmt acc -> acc {queryTailFormat = Just fmt})
                    | asciiEqCI "FOR" w ->
                        setUniqueMaybe queryTailFor "locking" parseForClause (\c acc -> acc {queryTailFor = Just c})
                    | asciiEqCI "INTO" w -> do
                        enabled <- lift (hasFeatureP FeatureSelectIntoUserVar)
                        when enabled $
                            setUniqueMaybeWithTok
                                queryTailInto
                                "INTO"
                                parseSelectInto
                                (\into acc -> acc {queryTailInto = Just into})
                _ -> pure ()

        setUniqueMaybe proj label p = setUniqueMaybeWithTok proj label (const p)
        setUniqueMaybeWithTok proj label p set = do
            cur <- gets proj
            tok <- lift advanceTok
            when (isJust cur) $ lift (unexpectedUnsupported tok ("duplicate " <> label <> " clause"))
            val <- lift (p tok)
            modify' (set val)
            go

        setUniqueList proj label p = setUniqueListWithTok proj label (const p)
        setUniqueListWithTok proj label p set = do
            cur <- gets proj
            tok <- lift advanceTok
            unless (null cur) $ lift (unexpectedUnsupported tok ("duplicate " <> label <> " clause"))
            val <- lift (p tok)
            modify' (set val)
            go

parseLimitClause :: P (Located LimitClause)
parseLimitClause = withLocated do
    first <- parseExpr
    mComma <- matchTok TokComma
    mSecond <- case mComma of
        Just commaTok -> do
            requireDialectP
                [MySQL, MariaDB, SQLite, ClickHouse]
                (locSpan commaTok)
                "LIMIT offset, count"
            Just <$> parseExpr
        Nothing -> pure Nothing
    pure (LimitClause first mSecond (isJust mSecond))

parseFetchClause :: P (Located FetchClause)
parseFetchClause = do
    start <- currentSpan
    requireDialectP [PostgreSQL, DuckDB] start "FETCH clause"
    withLocated do
        _ <- matchAnyWordCI ["FIRST", "NEXT"]
        mw <- currentHeadWord
        count <-
            if maybe False (wordIn ["ROW", "ROWS"]) mw
                then pure Nothing
                else peekTok >>= traverse (const parseExpr)
        _ <- matchAnyWordCI ["ROW", "ROWS"]
        ties <- evalContT do
            onWord "ONLY" (pure False)
            lift (expectWordsCI ["WITH", "TIES"] $> True)
        pure (FetchClause count ties)

-- | Parse a @FOR UPDATE/SHARE/...@ locking clause.
parseForClause :: P (Located ForClause)
parseForClause = do
    start <- currentSpan
    requireDialectP [PostgreSQL, MySQL, MariaDB] start "FOR locking clause"
    withLocated do
        strength <- evalContT do
            onWord "UPDATE" (pure ForUpdate)
            onWordTok "SHARE" \tok ->
                requireDialectP [PostgreSQL, MySQL] (locSpan tok) "FOR SHARE" $> ForShare
            onWordTok "NO" \tok -> do
                requireDialectP [PostgreSQL] (locSpan tok) "FOR NO KEY UPDATE"
                expectWordsCI ["KEY", "UPDATE"] $> ForNoKeyUpdate
            lift do
                tok <- expectWordCI "KEY"
                requireDialectP [PostgreSQL] (locSpan tok) "FOR KEY SHARE"
                expectWordCI "SHARE" $> ForKeyShare
        tables <-
            fromMaybe []
                <$> whenWordTok "OF" \ofTok ->
                    requireDialectP [PostgreSQL, MySQL] (locSpan ofTok) "FOR UPDATE/SHARE OF"
                        *> parseCommaSeparated parseName
        noWait <- isJust <$> matchWordCI "NOWAIT"
        skipLocked <- isJust <$> whenWord "SKIP" (expectWordCI "LOCKED")
        pure (ForClause strength tables skipLocked noWait)

parseQueryBody :: P (Located QueryBody)
parseQueryBody = parseIntersectBody >>= loopSet
    where
        loopSet lhs =
            matchAnyWordCI ["UNION", "EXCEPT"] >>= \case
                Nothing -> pure lhs
                Just (_, opWord) -> do
                    quantifier <- parseSetQuantifier
                    rhs <- parseIntersectBody
                    let op = if asciiEqCI "UNION" opWord then SetUnion else SetExcept
                    loopSet (L (mergeSpan (locSpan lhs) (locSpan rhs)) (QueryBodySet op quantifier lhs rhs))

parseIntersectBody :: P (Located QueryBody)
parseIntersectBody = parseQueryPrimary >>= loopIntersect
    where
        loopIntersect lhs =
            matchWordCI "INTERSECT" >>= \case
                Nothing -> pure lhs
                Just _ -> do
                    quantifier <- parseSetQuantifier
                    rhs <- parseQueryPrimary
                    loopIntersect
                        (L (mergeSpan (locSpan lhs) (locSpan rhs)) (QueryBodySet SetIntersect quantifier lhs rhs))

parseSetQuantifier :: P SetQuantifier
parseSetQuantifier = evalContT do
    onWord "ALL" (pure SetAll)
    onWord "DISTINCT" (pure SetDistinct)
    pure SetDefault

parseQueryPrimary :: P (Located QueryBody)
parseQueryPrimary = evalContT do
    onWordTok "SELECT" \tok -> locatedSpanning (locSpan tok) . QueryBodySelect =<< parseSelectCore
    onWordTok "VALUES" \tok -> locatedSpanning (locSpan tok) . QueryBodyValues =<< parseValueRows
    onWordTok "TABLE" \tok -> do
        requireDialectP [PostgreSQL, MySQL, MariaDB, DuckDB] (locSpan tok) "standalone TABLE query"
        locatedSpanning (locSpan tok) . QueryBodyTable =<< parseName
    onWordTok "FROM" \tok -> do
        requireDialectP [DuckDB, ClickHouse] (locSpan tok) "FROM-first query"
        fromClause <- parseFromClause
        (quantifier, items) <- fromMaybe (SelectDefault, []) <$> whenWord "SELECT" parseProjection
        core <- parseSelectCoreTail quantifier items Nothing (Just fromClause)
        locatedSpanning (locSpan tok) (QueryBodyFrom core)
    lift do
        peekToken >>= \case
            Just TokLParen -> withLocated (QueryBodyParen <$> inParens parseQuery)
            _ -> unexpectedHere "expected SELECT, VALUES, TABLE, or a parenthesized query"

-- | Parse @VALUES@ rows; MySQL requires @ROW(...)@, other dialects use parentheses.
parseValuesRows :: P [[Located Expr]]
parseValuesRows = expectWordCI "VALUES" *> parseValueRows

parseValueRows :: P [[Located Expr]]
parseValueRows = do
    mysql <- isDialect MySQL
    if mysql
        then parseCommaSeparated parseMySQLRow
        else parseCommaSeparated (parseParenList parseExpr)
    where
        parseMySQLRow = do
            _ <- expectWordCI "ROW"
            inParens (parseOptionalCommaList parseExpr)

parseGroupByItem :: P (Located GroupByItem)
parseGroupByItem = do
    isGroupingSets <- (&&) <$> wordIsCurrent "GROUPING" <*> wordIsAt 1 "SETS"
    isRollup <- keywordBeforeParen "ROLLUP"
    isCube <- keywordBeforeParen "CUBE"
    if isGroupingSets || isRollup || isCube
        then evalContT do
            onWordTok "GROUPING" \tok -> do
                _ <- expectWordCI "SETS"
                groupingConstruct tok "GROUPING SETS" GroupByGroupingSets
            onWordTok "ROLLUP" \tok -> groupingConstruct tok "ROLLUP" GroupByRollup
            onWordTok "CUBE" \tok -> groupingConstruct tok "CUBE" GroupByCube
            lift (unexpectedHere "expected GROUPING SETS, ROLLUP, or CUBE")
        else groupByExprItem
    where
        keywordBeforeParen keyword = do
            current <- wordIsCurrent keyword
            mnext <- peekTokAt 1
            pure (current && maybe False ((== TokLParen) . locValue) mnext)

        groupingConstruct tok what ctor = do
            requireDialectP [PostgreSQL, DuckDB, ClickHouse] (locSpan tok) what
            items <- parseParenList parseGroupByItem
            locatedSpanning (locSpan tok) (ctor items)

groupByExprItem :: P (Located GroupByItem)
groupByExprItem = groupByItemFromExpr <$> parseExpr

{- | Normalise parenthesised expressions into structural grouping sets so
@()@ and @(a, b)@ survive as grouping constructs rather than expression
wrappers.
-}
groupByItemFromExpr :: Located Expr -> Located GroupByItem
groupByItemFromExpr expr =
    case locValue expr of
        ExprParen inner -> L (locSpan expr) (GroupByParens [groupByItemFromExpr inner])
        ExprTuple items -> L (locSpan expr) (GroupByParens (map groupByItemFromExpr items))
        _ -> L (locSpan expr) (GroupByExpr expr)

parseSelectCore :: P SelectCore
parseSelectCore = do
    (quantifier, items) <- parseProjection
    into <- whenWordTok "INTO" parseSelectInto
    fromClause <- whenWord "FROM" parseFromClause
    parseSelectCoreTail quantifier items into fromClause

parseProjection :: P (SelectQuantifier, [Located SelectItem])
parseProjection = (,) <$> parseSelectQuantifier <*> parseSelectItems

parseSelectCoreTail ::
    SelectQuantifier ->
    [Located SelectItem] ->
    Maybe (Located SelectInto) ->
    Maybe FromClause ->
    P SelectCore
parseSelectCoreTail quantifier items into fromClause = do
    (whereExpr, prewhereExpr) <- parseWherePrewhere Nothing Nothing
    groupBy <-
        fromMaybe [] <$> whenWord "GROUP" (expectWordCI "BY" *> parseCommaSeparated parseGroupByItem)
    havingExpr <- whenWord "HAVING" parseExpr
    windows <- fromMaybe [] <$> whenWord "WINDOW" (parseCommaSeparated parseWindowDef)
    qualifyExpr <- whenWordTok "QUALIFY" \tok ->
        requireFeatureP FeatureQualifyClause (locSpan tok) "QUALIFY"
            *> (parseExpr >>= locatedSpanning (locSpan tok))
    pure
        SelectCore
            { selectQuantifier = quantifier
            , selectItems = items
            , selectInto = into
            , selectFrom = fromClause
            , selectWhere = whereExpr
            , selectPrewhere = prewhereExpr
            , selectGroupBy = groupBy
            , selectHaving = havingExpr
            , selectWindows = windows
            , selectQualify = qualifyExpr
            }

-- | Parse a PostgreSQL table destination or a MySQL/MariaDB variable list.
parseSelectInto :: SpannedToken -> P (Located SelectInto)
parseSelectInto tok = do
    dialect <- gets (dialectId . psProfile)
    subDepth <- gets psSubqueryDepth
    when (dialect == MariaDB && subDepth > 0) $
        throwParse
            ParseUnsupported
            (locSpan tok)
            "SELECT ... INTO is not allowed inside subqueries in MariaDB"
    intoVariables <- hasFeatureP FeatureSelectIntoUserVar
    destination <-
        if intoVariables
            then do
                targets <- parseCommaSeparated parseIntoTarget
                pure $ case partitionEithers targets of
                    (userVars, []) -> SelectIntoUserVars userVars
                    (_, firstSpVar : _) ->
                        SelectInto (L (locSpan firstSpVar) (Name [firstSpVar])) SelectIntoPersistent False
            else do
                requireFeatureP FeatureSelectIntoCreatesTable (locSpan tok) "SELECT INTO table"
                scope <- evalContT do
                    onWords ["TEMP", "TEMPORARY"] \_ _ -> pure SelectIntoTemporary
                    onWord "UNLOGGED" (pure SelectIntoUnlogged)
                    pure SelectIntoPersistent
                tableKeyword <- isJust <$> matchWordCI "TABLE"
                table <- parseName
                pure (SelectInto table scope tableKeyword)
    locatedSpanning (locSpan tok) destination
    where
        parseIntoTarget :: P (Either (Located ByteString) (Located NamePart))
        parseIntoTarget =
            peekTok >>= \case
                Just (L sp (TokUserVar raw)) -> advanceTok $> Left (L sp raw)
                Just mt@(L sp (TokWord wt))
                    | isJust (wordQuote wt)
                        || (tokenStartsBareAlias (Just mt) && not (wordIn ["OUTFILE", "DUMPFILE"] (wordRaw wt))) ->
                        advanceTok $> Right (L sp (NamePart (wordRaw wt) (isJust (wordQuote wt))))
                _ -> unexpectedHere "expected a variable in SELECT INTO"

parseSelectQuantifier :: P SelectQuantifier
parseSelectQuantifier = evalContT do
    onWord "ALL" (pure SelectAll)
    onWord "DISTINCT" $ evalContT do
        onWordTok "ON" \tok -> do
            requireDialectP [PostgreSQL, DuckDB, ClickHouse] (locSpan tok) "DISTINCT ON"
            SelectDistinctOn <$> parseParenList parseExpr
        pure SelectDistinct
    pure SelectDefault

parseSelectItems :: P [Located SelectItem]
parseSelectItems = do
    mt <- peekTok
    case mt of
        Nothing -> unexpectedHere "expected a projection expression"
        Just (L _ tok)
            | Just w <- headWordOf tok
            , isSelectClauseWord w ->
                unexpectedHere "expected a projection expression"
            | otherwise -> do
                item <- parseSelectItem
                (item :) <$> go (isJust (selectItemAlias (locValue item)))
    where
        go lastHadAlias =
            matchTok TokComma >>= \case
                Nothing -> pure []
                Just _ -> do
                    duck <- isDialect DuckDB
                    ch <- isDialect ClickHouse
                    let trailing = duck || (ch && not lastHadAlias)
                    mt <- peekTok
                    case mt of
                        Just tok
                            | isSelectListEnd tok ->
                                if trailing
                                    then pure []
                                    else unexpectedHere "expected a projection expression"
                        _ -> do
                            item <- parseSelectItem
                            (item :) <$> go (isJust (selectItemAlias (locValue item)))
        isSelectListEnd (L _ TokRParen) = True
        isSelectListEnd (L _ tok) = maybe False isSelectClauseWord (headWordOf tok)

isSelectClauseWord :: ByteString -> Bool
isSelectClauseWord =
    wordIn
        [ "FROM"
        , "WHERE"
        , "GROUP"
        , "HAVING"
        , "WINDOW"
        , "QUALIFY"
        , "PREWHERE"
        , "SETTINGS"
        , "ORDER"
        , "LIMIT"
        , "OFFSET"
        , "FETCH"
        , "FOR"
        , "UNION"
        , "INTERSECT"
        , "EXCEPT"
        , "INTO"
        ]

-- | Parse one projection item with an optional @AS@ or bare alias.
parseSelectItem :: P (Located SelectItem)
parseSelectItem = withLocated do
    expr <- parseExpr
    clickhouseFormat <- (&&) <$> isDialect ClickHouse <*> wordIsCurrent "FORMAT"
    if clickhouseFormat
        then pure (SelectItem expr Nothing False)
        else evalContT do
            onWord "AS" do
                alias <- parseAlias False
                pure (SelectItem expr (Just alias) True)
            lift do
                alias <- parseOptionalAlias False
                pure (SelectItem expr alias False)

parseWherePrewhere ::
    Maybe (Located Expr) ->
    Maybe (Located (Located Expr)) ->
    P (Maybe (Located Expr), Maybe (Located (Located Expr)))
parseWherePrewhere whereExpr prewhereExpr = evalContT do
    onWord "WHERE" do
        when (isJust whereExpr) (unexpectedHere "duplicate WHERE clause")
        expr <- parseExpr
        parseWherePrewhere (Just expr) prewhereExpr
    onWordTok "PREWHERE" \tok -> do
        requireFeatureP FeaturePrewhereSettings (locSpan tok) "PREWHERE"
        when (isJust prewhereExpr) (unexpectedHere "duplicate PREWHERE clause")
        expr <- parseExpr >>= locatedSpanning (locSpan tok)
        parseWherePrewhere whereExpr (Just expr)
    pure (whereExpr, prewhereExpr)

parseWindowDef :: P (Located WindowDef)
parseWindowDef = withLocated (WindowDef <$> parseNamePart <*> (expectWordCI "AS" *> inParens parseWindowSpec))

-- ---------------------------------------------------------------------------
-- FROM and joins
-- ---------------------------------------------------------------------------

-- | Parse a comma-separated FROM list.
parseFromClause :: P FromClause
parseFromClause = FromClause <$> parseCommaSeparated parseTableRef

-- | Parse one table source with ONLY/descendant markers, sample, alias, FINAL, and joins.
parseTableRef :: P (Located TableRef)
parseTableRef = withLocated do
    pg <- isDialect PostgreSQL
    onlyFlag <-
        if pg
            then do
                isOnly <- wordIsCurrent "ONLY"
                nextTok <- peekTokAt 1
                let startsTarget = case nextTok of
                        Just (L _ (TokWord wt))
                            | Just uw <- unquotedWord wt ->
                                not (asciiEqCI "AS" (wordRaw uw)) && tokenStartsBareAlias nextTok
                            | otherwise -> True
                        _ -> False
                if isOnly && startsTarget
                    then expectWordCI "ONLY" $> True
                    else pure False
            else pure False
    source <- parseTableSource
    includeDescendants <- case locValue source of
        TableSourceName _ ->
            matchTok TokStar >>= \case
                Just starTok -> do
                    requireDialectP [PostgreSQL] (locSpan starTok) "table * descendant marker"
                    pure True
                Nothing -> pure False
        _ -> pure False
    sample1 <- parseTableSample
    alias <- parseOptionalAlias True
    sample2 <- case sample1 of
        Just _ -> pure Nothing
        Nothing -> parseTableSample
    finalFlag <-
        isJust
            <$> whenWordTok "FINAL" \tok ->
                requireDialectP [ClickHouse] (locSpan tok) "FINAL modifier" $> ()
    clickhouse <- isDialect ClickHouse
    sample3 <- if clickhouse then parseTableSample else pure Nothing
    joins <- manyWhile isJoinStart parseJoinClause
    pure
        (TableRef onlyFlag source includeDescendants alias finalFlag (sample1 <|> sample2 <|> sample3) joins)
    where
        isJoinStart = do
            dialect <- gets (dialectId . psProfile)
            mw <- currentHeadWord
            pure $ case mw of
                Nothing -> False
                Just w ->
                    wordIn ["JOIN", "INNER", "LEFT", "RIGHT", "CROSS", "NATURAL", "ARRAY", "GLOBAL"] w
                        || (dialect `notElem` [MySQL, MariaDB] && asciiEqCI "FULL" w)
                        || (dialect `elem` [DuckDB, ClickHouse] && asciiEqCI "ASOF" w)
                        || (dialect == ClickHouse && wordIn ["ALL", "ANY", "SEMI", "ANTI"] w)

parseTableSample :: P (Maybe (Located TableSample))
parseTableSample = do
    clickhouse <- isDialect ClickHouse
    if clickhouse then parseClickHouseSample else parseTablesample
    where
        parseTablesample = whenWordTok "TABLESAMPLE" \tok -> do
            requireDialectP [PostgreSQL, DuckDB] (locSpan tok) "TABLESAMPLE"
            mw <- currentWord
            method <- case mw of
                Just (mTok, w) -> advanceTok $> L (locSpan mTok) w
                Nothing -> pure (L (locSpan tok) "SYSTEM")
            (amount, percent) <- evalContT do
                onTok TokLParen do
                    result <- sampleAmount
                    _ <- expectTok TokRParen
                    pure result
                lift sampleAmount
            repeatable <- whenWord "REPEATABLE" (inParens parseExpr)
            locatedSpanning
                (locSpan tok)
                (TableSample method amount percent repeatable Nothing)

        parseClickHouseSample = whenWordTok "SAMPLE" \tok -> do
            amount <- parseExpr
            offset <- whenWord "OFFSET" parseExpr
            locatedSpanning
                (locSpan tok)
                (TableSample (L (locSpan tok) "SAMPLE") amount False Nothing offset)

{- | Parse a TABLESAMPLE amount.  The expression stops above modulo precedence
so a trailing @%@ stays the sample unit instead of starting a binary modulo,
and the suffix is reported separately instead of being dropped.
-}
sampleAmount :: P (Located Expr, Bool)
sampleAmount = do
    amount <- parseExprPrec (precMul + 1)
    percent <- isJust <$> matchTok (TokOperator "%")
    pure (amount, percent)

parseJoinClause :: P (Located JoinClause)
parseJoinClause = withLocated do
    natural <- isJust <$> matchWordCI "NATURAL"
    mGlobalSpan <-
        whenWordTok "GLOBAL" \tok ->
            requireDialectP [ClickHouse] (locSpan tok) "GLOBAL JOIN" $> locSpan tok
    let globalFlag = isJust mGlobalSpan
    strictness <- fmap (\(L sp _, w) -> L sp w) <$> matchAnyWordCI ["ALL", "ANY", "SEMI", "ANTI"]
    traverse_
        (\(L sp _) -> requireDialectP [ClickHouse] sp "join strictness")
        strictness
    ParsedJoinKind {pjkKind = kind, pjkExplicitInner = explicitInner, pjkAsofBeforeDir = asofBeforeDir} <-
        parseJoinKind
    let isJoinArray (JoinArray _) = True
        isJoinArray _ = False
    traverse_
        ( \gSp -> when (isJoinArray kind) (throwParse ParseUnsupported gSp "GLOBAL ARRAY JOIN is not supported")
        )
        mGlobalSpan
    case strictness of
        Nothing -> pure ()
        Just (L strictSp strictRaw) -> do
            when natural $
                throwParse ParseUnsupported strictSp "NATURAL JOIN does not support join strictness"
            when (kind == JoinCross || isJoinArray kind) $
                throwParse ParseUnsupported strictSp "CROSS/ARRAY JOIN does not support join strictness"
            when asofBeforeDir $
                throwParse
                    ParseUnsupported
                    strictSp
                    "ASOF must follow LEFT/RIGHT when combined with join strictness"
            when (wordIn ["SEMI", "ANTI"] strictRaw && (explicitInner || kind == JoinFull)) $
                throwParse ParseUnsupported strictSp "SEMI/ANTI JOIN only supports JOIN, LEFT, RIGHT, or ASOF"
    target <- case kind of
        JoinArray bool -> JoinTargetArray <$> parseArrayJoinItems bool
        _ -> JoinTargetTable <$> parseTableRef
    constraint <-
        if natural
            then do
                when (kind == JoinCross) do
                    sp <- currentSpan
                    requireDialectP [SQLite] sp "NATURAL CROSS JOIN"
                peekTok >>= traverse_ \(L sp tok) ->
                    when (maybe False (wordIn ["ON", "USING"]) (headWordOf tok)) $
                        throwParse ParseUnsupported sp "NATURAL JOIN does not accept ON/USING"
                pure Nothing
            else case kind of
                JoinArray _ -> pure Nothing
                JoinCross -> do
                    dialect <- gets (dialectId . psProfile)
                    if dialect `elem` [SQLite, MySQL, MariaDB]
                        then parseJoinConstraint
                        else do
                            peekTok >>= traverse_ \(L sp tok) ->
                                when (maybe False (wordIn ["ON", "USING"]) (headWordOf tok)) $
                                    throwParse ParseUnsupported sp "CROSS JOIN does not accept ON/USING"
                            pure Nothing
                _ -> do
                    mConstraint <- parseJoinConstraint
                    when (isNothing mConstraint) do
                        dialect <- gets (dialectId . psProfile)
                        let needsCondition =
                                dialect `elem` [PostgreSQL, DuckDB, ClickHouse]
                                    || case kind of
                                        JoinLeft -> dialect `elem` [MySQL, MariaDB]
                                        JoinRight -> dialect `elem` [MySQL, MariaDB]
                                        JoinAsof _ -> True
                                        _ -> False
                        when needsCondition $
                            unexpectedHere "JOIN requires ON or USING"
                    pure mConstraint
    pure (JoinClause natural globalFlag strictness kind target constraint)

data ParsedJoinKind = ParsedJoinKind
    { pjkKind :: !JoinKind
    , pjkExplicitInner :: !Bool
    , pjkAsofBeforeDir :: !Bool
    }

parseJoinKind :: P ParsedJoinKind
parseJoinKind = evalContT do
    onWord "JOIN" (pure (ParsedJoinKind JoinInner False False))
    onWord "INNER" (expectWordCI "JOIN" $> ParsedJoinKind JoinInner True False)
    onWordTok "LEFT" \tok -> evalContT do
        onWord "ARRAY" do
            requireDialectP [ClickHouse] (locSpan tok) "LEFT ARRAY JOIN"
            expectWordCI "JOIN" $> ParsedJoinKind (JoinArray True) False False
        onWord "ASOF" do
            requireDialectP [ClickHouse] (locSpan tok) "LEFT ASOF JOIN"
            expectWordCI "JOIN" $> ParsedJoinKind (JoinAsof False) False False
        lift (matchWordCI "OUTER" *> expectWordCI "JOIN" $> ParsedJoinKind JoinLeft False False)
    onWord "RIGHT" (matchWordCI "OUTER" *> expectWordCI "JOIN" $> ParsedJoinKind JoinRight False False)
    onWordTok "FULL" \tok -> do
        requireDialectP [PostgreSQL, SQLite, DuckDB, ClickHouse] (locSpan tok) "FULL JOIN"
        matchWordCI "OUTER" *> expectWordCI "JOIN" $> ParsedJoinKind JoinFull False False
    onWord "CROSS" (expectWordCI "JOIN" $> ParsedJoinKind JoinCross False False)
    onWordTok "ARRAY" \tok -> do
        requireDialectP [ClickHouse] (locSpan tok) "ARRAY JOIN"
        expectWordCI "JOIN" $> ParsedJoinKind (JoinArray False) False False
    onWordTok "ASOF" \tok -> do
        requireDialectP [DuckDB, ClickHouse] (locSpan tok) "ASOF JOIN"
        mRight <- matchWordCI "RIGHT"
        traverse_ (\rTok -> requireDialectP [DuckDB] (locSpan rTok) "ASOF RIGHT JOIN") mRight
        mLeft <- if isJust mRight then pure Nothing else matchWordCI "LEFT"
        expectWordCI "JOIN"
            $> ParsedJoinKind (JoinAsof (isJust mRight)) False (isJust mRight || isJust mLeft)
    lift (expectWordCI "JOIN" >>= (`unexpectedUnsupported` "expected JOIN"))

parseArrayJoinItems :: Bool -> P [Located ArrayJoinItem]
parseArrayJoinItems _ = parseCommaSeparated parseArrayJoinItem

parseArrayJoinItem :: P (Located ArrayJoinItem)
parseArrayJoinItem = withLocated do
    expr <- parseExpr
    alias <- evalContT do
        onWord "AS" (Just <$> parseNamePart)
        lift do
            mt <- peekTok
            if tokenStartsBareAlias mt then Just <$> parseNamePart else pure Nothing
    pure (ArrayJoinItem expr alias)

parseJoinConstraint :: P (Maybe JoinConstraint)
parseJoinConstraint = evalContT do
    onWord "ON" (Just . JoinOn <$> parseExpr)
    onWord "USING" (Just . JoinUsing <$> parseParenList parseNamePart)
    pure Nothing

parseTableSource :: P (Located TableSource)
parseTableSource = evalContT do
    onWordTok "LATERAL" \tok -> do
        requireDialectP
            [PostgreSQL, MySQL, DuckDB]
            (locSpan tok)
            "LATERAL table source"
        parseTableSourceBase >>= locatedSpanning (locSpan tok) . TableSourceLateral
    lift parseTableSourceBase

parseTableSourceBase :: P (Located TableSource)
parseTableSourceBase = do
    mt <- peekTok
    case mt of
        Just (L _ TokLParen) -> withLocated $ inParens do
            isQuery <- queryStartsAt 0
            if isQuery
                then TableSourceSubquery <$> inSubqueryP parseQuery
                else TableSourceParens <$> parseFromClause
        Just (L tokSp (TokWord wt))
            | Just uw <- unquotedWord wt
            , asciiEqCI "VALUES" (wordRaw uw) -> do
                requireDialectP [DuckDB] tokSp "FROM VALUES"
                rows <- parseValuesRows
                locatedSpanning tokSp (TableSourceValues rows)
            | otherwise -> do
                name <- parseName
                next <- peekTok
                case next of
                    Just (L _ TokLParen) -> do
                        mysqlLike <- (||) <$> isDialect MySQL <*> isDialect MariaDB
                        when (mysqlLike && not (isMySQLTableFunction (locValue name))) $
                            throwParse
                                ParseUnsupported
                                (locSpan name)
                                "table-valued functions in FROM are not supported in MySQL/MariaDB"
                        rewrap TableSourceFunction <$> parseFunctionCallTail name
                    _ -> pure (L (locSpan name) (TableSourceName name))
        _ -> unexpectedHere "expected a table name or subquery"
    where
        isMySQLTableFunction (Name [L _ (NamePart raw False)]) = asciiEqCI "JSON_TABLE" raw
        isMySQLTableFunction _ = False
