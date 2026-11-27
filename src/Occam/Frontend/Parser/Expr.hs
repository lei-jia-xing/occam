-- | Pratt expression parser plus type names, windows, ordering, and index keys.
module Occam.Frontend.Parser.Expr (
    precOther,
    precMul,
    parseExpr,
    parseExprPrec,
    parseTypeName,
    parseWindowSpec,
    parseWindowRef,
    parseOrderItem,
    parseSortDirection,
    parseNullsOrder,
    parseSettingItem,
    parseFunctionCallTail,
    parseIndexKeyList,
    parseIndexKeyPart,
    parseOptionalConstraintName,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard, join, replicateM, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.State.Strict (gets)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (find, traverse_)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Occam.Frontend.AST
import Occam.Frontend.Lexer hiding (LiteralKind)
import Occam.Frontend.Parser.Core
import Occam.Frontend.Span (ByteSpan (..), Located (..), mergeSpan, spanEnd, spanStart)
import Occam.Parser.Dialect (DialectFeature (..), DialectProfile (..))
import Occam.Parser.Types (SQLDialect (..))

-- ---------------------------------------------------------------------------
-- Pratt expression parser
-- ---------------------------------------------------------------------------

-- | Binding powers for the Pratt loop; higher binds tighter.
precOr
    , precXor
    , precAnd
    , precNot
    , precCompare
    , precOther
    , precConcat
    , precBitOr
    , precBitAnd
    , precShift
    , precAdd
    , precMul
    , precPow
    , precUnary
    , precCast ::
        Int
precOr = 1
precXor = 2
precAnd = 3
precNot = 4
precCompare = 5
precOther = 6
precBitOr = 7
precBitAnd = 8
precShift = 9
precConcat = 10
precAdd = 11
precMul = 12
precPow = 13
precUnary = 14
precCast = 15

-- | Parse a complete expression at the lowest precedence.
parseExpr :: P (Located Expr)
parseExpr = parseExprPrec 0

-- | Parse an expression whose operators bind at least as tightly as @minPrec@.
parseExprPrec :: Int -> P (Located Expr)
parseExprPrec minPrec = parsePrefix minPrec >>= infixLoop minPrec

infixLoop :: Int -> Located Expr -> P (Located Expr)
infixLoop minPrec lhs =
    tryPostfix minPrec lhs >>= \case
        Just next -> infixLoop minPrec next
        Nothing ->
            peekInfix >>= \case
                Just desc
                    | infixDescPrecedence desc >= minPrec ->
                        parseInfix desc lhs >>= infixLoop minPrec
                _ -> pure lhs

data InfixKind
    = InfixBinary !BinaryOp
    | InfixIs
    | InfixBetween !Bool
    | InfixIn !Bool
    | InfixLike !Bool !LikeOp

data InfixDesc = InfixDesc
    { infixDescKind :: !InfixKind
    , infixDescPrecedence :: !Int
    , infixDescRightAssoc :: !Bool
    }

peekInfix :: P (Maybe InfixDesc)
peekInfix = runMaybeT (wordInfix <|> tokInfix)
    where
        plain op prec = InfixDesc (InfixBinary op) prec (op == OpPow)
        predDesc kind = InfixDesc kind precCompare False

        wordInfix = do
            (_, raw) <- MaybeT currentWord
            case find (\(w, _) -> asciiEqCI w raw) wordTable of
                Just (_, desc) -> pure desc
                Nothing -> do
                    guard (asciiEqCI "NOT" raw)
                    (_, nextRaw) <- MaybeT (wordAt 1)
                    (_, kind) <- MaybeT . pure $ find (\(w, _) -> asciiEqCI w nextRaw) notWordTable
                    pure (predDesc kind)

        tokInfix = do
            L _ tok <- MaybeT peekTok
            dialect <- lift (gets (dialectId . psProfile))
            MaybeT (pure (tokenInfix dialect tok))

        wordTable =
            [ ("OR", plain OpOr precOr)
            , ("AND", plain OpAnd precAnd)
            , ("XOR", plain OpXor precXor)
            , ("IS", predDesc InfixIs)
            , ("BETWEEN", predDesc (InfixBetween False))
            , ("IN", predDesc (InfixIn False))
            , ("LIKE", predDesc (InfixLike False LikeLike))
            , ("ILIKE", predDesc (InfixLike False LikeILike))
            , ("GLOB", predDesc (InfixLike False LikeGlob))
            , ("REGEXP", predDesc (InfixLike False LikeRegexp))
            , ("RLIKE", predDesc (InfixLike False LikeRegexp))
            ]

        notWordTable =
            [ ("BETWEEN", InfixBetween True)
            , ("IN", InfixIn True)
            , ("LIKE", InfixLike True LikeLike)
            , ("ILIKE", InfixLike True LikeILike)
            , ("GLOB", InfixLike True LikeGlob)
            , ("REGEXP", InfixLike True LikeRegexp)
            , ("RLIKE", InfixLike True LikeRegexp)
            ]

tokenInfix :: SQLDialect -> Token -> Maybe InfixDesc
tokenInfix dialect = \case
    TokEq -> plain OpEq precCompare
    TokPlus -> plain OpAdd precAdd
    TokMinus -> plain OpSub precAdd
    TokStar -> plain OpMul precMul
    TokSlash -> plain OpDiv precMul
    TokArrow -> plain OpJsonGet precConcat
    TokDoubleArrow -> plain OpJsonGetText precConcat
    TokColonEq -> Just (InfixDesc (InfixBinary (OpCustom ":=")) 0 True)
    TokOperator raw -> operatorInfix dialect raw
    _ -> Nothing
    where
        plain op prec = Just (InfixDesc (InfixBinary op) prec (op == OpPow))

operatorInfix :: SQLDialect -> ByteString -> Maybe InfixDesc
operatorInfix dialect = \case
    "<" -> binary OpLt precCompare
    "<=" -> binary OpLtEq precCompare
    ">" -> binary OpGt precCompare
    ">=" -> binary OpGtEq precCompare
    "<>" -> binary OpNotEq precCompare
    "!=" -> binary OpNotEq precCompare
    "~~" -> binary (OpCustom "~~") precCompare
    "!~~" -> binary (OpCustom "!~~") precCompare
    "~~*" -> binary (OpCustom "~~*") precCompare
    "!~~*" -> binary (OpCustom "!~~*") precCompare
    "~" -> binary OpRegexMatch precCompare
    "~*" -> binary OpRegexIMatch precCompare
    "!~" -> binary OpRegexNotMatch precCompare
    "!~*" -> binary OpRegexNotIMatch precCompare
    "||" -> binary OpConcat precConcat
    "&&" -> binary OpOverlap precConcat
    "@>" -> binary OpContains precConcat
    "<@" -> binary OpContainedBy precConcat
    "->" -> binary OpJsonGet precConcat
    "->>" -> binary OpJsonGetText precConcat
    "#>" -> binary OpJsonPathGet precConcat
    "#>>" -> binary OpJsonPathGetText precConcat
    "%" -> binary OpMod precMul
    "^"
        | dialect `elem` [MySQL, MariaDB] -> binary OpBitXor precPow
        | otherwise -> binary OpPow precPow
    "**" -> binary OpPow precPow
    "<<" -> binary OpShiftLeft (bitwise precShift)
    ">>" -> binary OpShiftRight (bitwise precShift)
    "|" -> binary OpBitOr (bitwise precBitOr)
    "&" -> binary OpBitAnd (bitwise precBitAnd)
    "#" -> binary OpBitXor (bitwise precBitAnd)
    raw -> binary (OpCustom raw) precOther
    where
        binary op prec = Just (InfixDesc (InfixBinary op) prec (op == OpPow))
        bitwise prec
            | dialect `elem` [PostgreSQL, DuckDB, SQLite] = precOther
            | otherwise = prec

parseInfix :: InfixDesc -> Located Expr -> P (Located Expr)
parseInfix desc lhs = case infixDescKind desc of
    InfixBinary op -> do
        _ <- advanceTok
        when (op == OpCustom ":=") $
            requireDialectP [MySQL, MariaDB] (locSpan lhs) "':=' assignment"
        mQuantifier <- currentHeadWord
        case mQuantifier of
            Just q
                | isComparisonOp op
                , wordIn ["ANY", "ALL", "SOME"] q -> do
                    _ <- advanceTok
                    let quant
                            | asciiEqCI "ALL" q = QuantAll
                            | asciiEqCI "SOME" q = QuantSome
                            | otherwise = QuantAny
                    rhs <- parseAnyAllOperand
                    pure (L (mergeSpan (locSpan lhs) (locSpan rhs)) (ExprQuantified quant op lhs rhs))
            _ -> do
                let nextPrec =
                        if infixDescRightAssoc desc
                            then infixDescPrecedence desc
                            else infixDescPrecedence desc + 1
                rhs <- parseExprPrec nextPrec
                pure (L (mergeSpan (locSpan lhs) (locSpan rhs)) (ExprBinary op lhs rhs))
    InfixIs -> do
        _ <- expectWordCI "IS"
        negated <- isJust <$> matchWordCI "NOT"
        evalContT do
            onWord "DISTINCT" do
                _ <- expectWordCI "FROM"
                rhs <- parseExprPrec (precCompare + 1)
                pure (L (mergeSpan (locSpan lhs) (locSpan rhs)) (ExprIsDistinct negated lhs rhs))
            onWordTok "NULL" \tok -> pure (L (mergeSpan (locSpan lhs) (locSpan tok)) (ExprIsNull negated lhs))
            onWordTok "TRUE" \tok -> pure (L (mergeSpan (locSpan lhs) (locSpan tok)) (ExprIsTrue negated lhs))
            onWordTok "FALSE" \tok -> pure (L (mergeSpan (locSpan lhs) (locSpan tok)) (ExprIsFalse negated lhs))
            onWordTok "UNKNOWN" \tok -> pure (L (mergeSpan (locSpan lhs) (locSpan tok)) (ExprIsUnknown negated lhs))
            lift (unexpectedHere "expected NULL, TRUE, FALSE, UNKNOWN, or DISTINCT FROM after IS")
    InfixBetween negated -> do
        when negated (void (expectWordCI "NOT"))
        _ <- expectWordCI "BETWEEN"
        low <- parseExprPrec (precCompare + 1)
        _ <- expectWordCI "AND"
        high <- parseExprPrec (precCompare + 1)
        pure (L (mergeSpan (locSpan lhs) (locSpan high)) (ExprBetween negated lhs low high))
    InfixIn negated -> do
        when negated (void (expectWordCI "NOT"))
        _ <- expectWordCI "IN"
        predicate <- inParens do
            isQuery <- queryStartsAt 0
            if isQuery
                then ExprInSubquery negated lhs <$> parseQueryHook
                else do
                    mClose <- peekToken
                    items <- case mClose of
                        Just TokRParen -> do
                            requireDialectP [SQLite] (locSpan lhs) "empty IN () list"
                            pure []
                        _ -> parseCommaSeparated parseExpr
                    pure (ExprInList negated lhs items)
        locatedSpanning (locSpan lhs) predicate
    InfixLike negated likeOp -> do
        when negated (void (expectWordCI "NOT"))
        _ <- expectWordCI (likeOpWord likeOp)
        rhs <- parseExprPrec (precCompare + 1)
        escapeExpr <- whenWord "ESCAPE" (parseExprPrec (precCompare + 1))
        let endLoc = maybe (locSpan rhs) locSpan escapeExpr
        pure
            (L (ByteSpan (spanStart (locSpan lhs)) (spanEnd endLoc)) (ExprLike negated likeOp lhs rhs escapeExpr))

likeOpWord :: LikeOp -> ByteString
likeOpWord = \case
    LikeLike -> "LIKE"
    LikeILike -> "ILIKE"
    LikeGlob -> "GLOB"
    LikeRegexp -> "REGEXP"

isComparisonOp :: BinaryOp -> Bool
isComparisonOp = (`elem` [OpEq, OpNotEq, OpLt, OpLtEq, OpGt, OpGtEq])

parseAnyAllOperand :: P (Located Expr)
parseAnyAllOperand = withLocated $ inParens do
    isQuery <- queryStartsAt 0
    if isQuery
        then ExprSubquery <$> parseQueryHook
        else ExprParen <$> parseExpr

tryPostfix :: Int -> Located Expr -> P (Maybe (Located Expr))
tryPostfix minPrec lhs = runMaybeT do
    guard (precCast >= minPrec)
    L _ tok <- MaybeT peekTok
    case tok of
        TokDoubleColon -> lift do
            t <- expectTok TokDoubleColon
            requireFeatureP FeatureColonCast (locSpan t) ":: cast"
            typeName <- parseTypeName
            pure (L (mergeSpan (locSpan lhs) (locSpan typeName)) (ExprCast CastDoubleColon lhs typeName))
        TokDotColon -> lift do
            t <- expectTok TokDotColon
            requireDialectP [ClickHouse] (locSpan t) ".: typed path cast"
            typeName <- parseTypeName
            pure (L (mergeSpan (locSpan lhs) (locSpan typeName)) (ExprCast CastDotColon lhs typeName))
        TokLBracket -> lift do
            _ <- expectTok TokLBracket
            mt <- peekToken
            lower <- if mt == Just TokColon || mt == Just TokRBracket then pure Nothing else Just <$> parseExpr
            upper <-
                join <$> whenTok TokColon do
                    mt2 <- peekToken
                    if mt2 == Just TokRBracket then pure Nothing else Just <$> parseExpr
            close <- expectTok TokRBracket
            let sp = ByteSpan (spanStart (locSpan lhs)) (spanEnd (locSpan close))
            pure (L sp (ExprSubscript lhs lower upper))
        TokDot -> lift do
            _ <- expectTok TokDot
            peekTok >>= \case
                Just (L sp TokStar) -> do
                    requireDialectP [PostgreSQL] sp "composite .* expansion"
                    advanceTok *> locatedSpanning (locSpan lhs) (ExprFieldStar lhs)
                Just (L sp (TokLiteral LitNumber raw))
                    | BS.all (\b -> b >= 48 && b <= 57) raw -> do
                        requireDialectP [ClickHouse] sp "tuple field index"
                        advanceTok *> locatedSpanning (locSpan lhs) (ExprFieldIndex lhs (L sp raw))
                _ -> do
                    part <- parseNamePart
                    locatedSpanning (locSpan lhs) (ExprField lhs part)
        TokWord wt
            | Just uw <- unquotedWord wt
            , asciiEqCI "COLLATE" (wordRaw uw) -> do
                next <- lift (peekTokAt 1)
                guard (fmap locValue next /= Just TokEq)
                lift do
                    _ <- advanceTok
                    collation <- parseName
                    pure (L (mergeSpan (locSpan lhs) (locSpan collation)) (ExprCollate lhs collation))
        _ -> MaybeT (pure Nothing)

parsePrefix :: Int -> P (Located Expr)
parsePrefix minPrec = do
    mt <- peekTok
    case mt of
        Nothing -> unexpectedHere "expected an expression"
        Just (L sp tok) -> case tok of
            TokLiteral kind raw -> patternExpr sp (ExprLiteral (Literal kind raw))
            TokParam raw -> patternExpr sp (ExprParam ParamPositional raw)
            TokQuestion -> patternExpr sp (ExprParam ParamPositional "?")
            TokUserVar raw -> patternExpr sp (ExprParam ParamUser raw)
            TokSysVar raw -> patternExpr sp (ExprParam ParamSystem raw)
            TokStar -> patternExpr sp ExprStar
            TokPlus -> unaryExpr UnaryPositive precUnary
            TokMinus -> unaryExpr UnaryNegate precUnary
            TokOperator "~" -> unaryExpr UnaryBitNot precUnary
            TokLParen -> parseParenExpr
            TokLBracket -> parseBracketArray
            TokLBrace -> parseStructLiteral
            TokWord wt -> case unquotedWord wt of
                Just uw -> parseWordExpr minPrec sp (wordRaw uw)
                Nothing -> parseName >>= nameSuffix
            _ -> unexpectedHere "expected an expression"
    where
        patternExpr sp value = advanceTok $> L sp value
        unaryExpr op prec = do
            start <- currentSpan
            _ <- advanceTok
            operand <- parseExprPrec prec
            pure (L (ByteSpan (spanStart start) (spanEnd (locSpan operand))) (ExprUnary op operand))

parseWordExpr :: Int -> ByteSpan -> ByteString -> P (Located Expr)
parseWordExpr minPrec sp raw
    | asciiEqCI "NULL" raw = advanceTok $> L sp ExprNull
    | asciiEqCI "TRUE" raw = advanceTok $> L sp (ExprBoolean True)
    | asciiEqCI "FALSE" raw = advanceTok $> L sp (ExprBoolean False)
    | asciiEqCI "DEFAULT" raw = advanceTok $> L sp ExprDefault
    | asciiEqCI "NOT" raw = do
        mysqlLike <- gets ((`elem` [MySQL, MariaDB]) . dialectId . psProfile)
        when (mysqlLike && minPrec > precNot) $
            throwParse ParseUnexpectedToken sp "NOT requires parentheses in this operand"
        _ <- advanceTok
        operand <- parseExprPrec precNot
        locatedSpanning sp (ExprUnary UnaryNot operand)
    | asciiEqCI "CASE" raw = withLocated (ExprCase <$> parseCaseExpr)
    | asciiEqCI "CAST" raw = withLocated parseCastExpr
    | asciiEqCI "TRY_CAST" raw = do
        mOpen <- peekTokAt 1
        case mOpen of
            Just (L _ TokLParen) -> withLocated parseTryCastExpr
            _ -> parseName >>= nameSuffix
    | asciiEqCI "POSITION" raw = do
        mOpen <- peekTokAt 1
        case mOpen of
            Just (L _ TokLParen) -> withLocated parsePositionExpr
            _ -> parseName >>= nameSuffix
    | asciiEqCI "EXISTS" raw = withLocated parseExistsExpr
    | asciiEqCI "ARRAY" raw = do
        mNext <- peekTokAt 1
        case mNext of
            Just (L _ TokLBracket) -> withLocated (advanceTok *> (ExprArray <$> parseBracketArrayItems))
            Just (L _ TokLParen) -> withLocated (advanceTok *> (ExprArray <$> parseOptionalParenList parseExpr))
            _ -> parseName >>= nameSuffix
    | wordIn ["DATE", "TIME", "TIMESTAMP"] raw = do
        mNext <- peekTokAt 1
        case mNext of
            Just (L litSp (TokLiteral LitString litRaw)) -> do
                _ <- advanceTok
                _ <- advanceTok
                pure (L (mergeSpan sp litSp) (ExprTypedString raw (L litSp (Literal LitString litRaw))))
            _ -> parseName >>= nameSuffix
    | BS.isPrefixOf "_" raw = do
        mysql <- isDialect MySQL
        mariadb <- isDialect MariaDB
        mNext <- peekTokAt 1
        case mNext of
            Just (L litSp (TokLiteral kind litRaw))
                | (mysql || mariadb) && kind /= LitNumber -> do
                    _ <- advanceTok
                    _ <- advanceTok
                    pure (L (mergeSpan sp litSp) (ExprCharsetString raw (L litSp (Literal kind litRaw))))
            _ -> parseNameOrLambda
    | otherwise = parseNameOrLambda
    where
        parseNameOrLambda = do
            ch <- isDialect ClickHouse
            mArrow <- peekTokAt 1
            case mArrow of
                Just (L _ TokArrow) | ch -> do
                    param <- parseNamePart
                    _ <- expectTok TokArrow
                    body <- parseExpr
                    pure (L (mergeSpan (locSpan param) (locSpan body)) (ExprLambda [param] body))
                _ -> parseName >>= nameSuffix

nameSuffix :: Located Name -> P (Located Expr)
nameSuffix name = do
    mDot <- peekTok
    mNext <- peekTokAt 1
    case (mDot, mNext) of
        (Just (L _ TokDot), Just (L _ TokStar)) -> do
            _ <- advanceTok
            star <- advanceTok
            pure (L (mergeSpan (locSpan name) (locSpan star)) (ExprQualifiedStar name))
        (Just (L _ TokLParen), _) -> rewrap ExprCall <$> parseFunctionCallTail name
        _ -> pure (L (locSpan name) (ExprColumn name))

parseParenExpr :: P (Located Expr)
parseParenExpr = do
    parenExpr <- withLocated $ inParens do
        isQuery <- queryStartsAt 0
        if isQuery
            then ExprSubquery <$> parseQueryHook
            else do
                mClose <- peekToken
                case mClose of
                    Just TokRParen -> pure (ExprTuple [])
                    _ -> do
                        first <- parseExpr
                        matchTok TokComma >>= \case
                            Nothing -> pure (ExprParen first)
                            Just _ -> ExprTuple . (first :) <$> parseCommaSeparated parseExpr
    ch <- isDialect ClickHouse
    mArrow <- peekTok
    case (mArrow, extractLambdaParams (locValue parenExpr)) of
        (Just (L _ TokArrow), Just params) | ch -> do
            _ <- advanceTok
            body <- parseExpr
            pure (L (mergeSpan (locSpan parenExpr) (locSpan body)) (ExprLambda params body))
        _ -> pure parenExpr
    where
        extractLambdaParams = \case
            ExprTuple [] -> Just []
            ExprParen item -> (: []) <$> unwrapLambdaParam item
            ExprTuple items -> traverse unwrapLambdaParam items
            _ -> Nothing
        unwrapLambdaParam = \case
            L _ (ExprColumn (L _ (Name [part]))) -> Just part
            _ -> Nothing

parseBracketArray :: P (Located Expr)
parseBracketArray = withLocated (ExprArray <$> parseBracketArrayItems)

parseBracketArrayItems :: P [Located Expr]
parseBracketArrayItems = do
    _ <- expectTok TokLBracket
    mClose <- peekToken
    items <- case mClose of
        Just TokRBracket -> pure []
        _ -> parseCommaSeparated parseExpr
    _ <- expectTok TokRBracket
    pure items

-- | DuckDB struct literal @{ 'k': expr, ... }@.
parseStructLiteral :: P (Located Expr)
parseStructLiteral = withLocated do
    lbrace <- expectTok TokLBrace
    requireDialectP [DuckDB] (locSpan lbrace) "struct literal"
    mClose <- peekToken
    fields <- case mClose of
        Just TokRBrace -> pure []
        _ -> parseCommaSeparated parseStructField
    _ <- expectTok TokRBrace
    pure (ExprStruct fields)
    where
        parseStructField = do
            key <- parseExpr
            _ <- expectTok TokColon
            val <- parseExpr
            pure (key, val)

parseCaseExpr :: P CaseExpr
parseCaseExpr = do
    _ <- expectWordCI "CASE"
    mWhen <- wordIsCurrent "WHEN"
    operand <- if mWhen then pure Nothing else Just <$> parseExpr
    whens <- manyPrefix (matchWordCI "WHEN") ((,) <$> parseExpr <*> (expectWordCI "THEN" *> parseExpr))
    elseExpr <- whenWord "ELSE" parseExpr
    _ <- expectWordCI "END"
    pure (CaseExpr operand whens elseExpr)

-- | @CAST(expr AS type)@, plus ClickHouse's @CAST(expr, type)@.
parseCastExpr :: P Expr
parseCastExpr =
    expectWordCI "CAST" *> inParens do
        expr <- parseExpr
        matchTok TokComma >>= \case
            Just _ -> do
                requireDialectP [ClickHouse] (locSpan expr) "two-argument CAST(expr, type)"
                ExprCastExpr expr <$> parseExpr
            Nothing -> ExprCast CastFunction expr <$> (expectWordCI "AS" *> parseTypeName)

-- | DuckDB @TRY_CAST(expr AS type)@.
parseTryCastExpr :: P Expr
parseTryCastExpr = do
    tok <- expectWordCI "TRY_CAST"
    requireDialectP [DuckDB] (locSpan tok) "TRY_CAST"
    inParens do
        expr <- parseExpr
        ExprCast CastTryFunction expr <$> (expectWordCI "AS" *> parseTypeName)

parsePositionExpr :: P Expr
parsePositionExpr = do
    posName <- parseName
    _ <- expectTok TokLParen
    mClose <- peekToken
    case mClose of
        Just TokRParen -> ExprCall <$> finishFunctionCall posName False [] []
        _ -> do
            firstExpr0 <- parseExprPrec (precCompare + 1)
            mIn <- matchWordCI "IN"
            case mIn of
                Just inTok -> do
                    requireDialectP
                        [PostgreSQL, MySQL, MariaDB, DuckDB]
                        (locSpan inTok)
                        "POSITION(... IN ...)"
                    haystack <- parseExpr
                    let toArg e = L (locSpan e) (FunctionArgExpr e)
                    ExprCall <$> finishFunctionCall posName False [toArg firstExpr0, toArg haystack] []
                Nothing -> do
                    firstExpr <- infixLoop 0 firstExpr0
                    let firstArg = L (locSpan firstExpr) (FunctionArgExpr firstExpr)
                    restArgs <- fromMaybe [] <$> whenTok TokComma (parseCommaSeparated parseFunctionArg)
                    orderBy <-
                        fromMaybe [] <$> whenWord "ORDER" (expectWordCI "BY" *> parseCommaSeparated parseOrderItem)
                    ExprCall <$> finishFunctionCall posName False (firstArg : restArgs) orderBy

parseExistsExpr :: P Expr
parseExistsExpr = expectWordCI "EXISTS" *> (ExprExists <$> inParens parseQueryHook)

-- | Parse arguments, ordering, FILTER, NULL treatment, and OVER after the name.
parseFunctionCallTail :: Located Name -> P (Located FunctionCall)
parseFunctionCallTail name = do
    _ <- expectTok TokLParen
    distinct <- isJust <$> matchWordCI "DISTINCT"
    _ <- matchWordCI "ALL"
    args <- parseOptionalCommaList parseFunctionArg
    orderBy <-
        fromMaybe [] <$> whenWord "ORDER" (expectWordCI "BY" *> parseCommaSeparated parseOrderItem)
    finishFunctionCall name distinct args orderBy

finishFunctionCall ::
    Located Name -> Bool -> [Located FunctionArg] -> [Located OrderItem] -> P (Located FunctionCall)
finishFunctionCall name distinct args orderBy = do
    _ <- expectTok TokRParen
    isWithinGroup <- (&&) <$> wordIsCurrent "WITHIN" <*> wordIsAt 1 "GROUP"
    (withinTok, withinGroup) <-
        if isWithinGroup
            then do
                tok <- expectWordCI "WITHIN"
                requireDialectP [PostgreSQL, DuckDB, MariaDB] (locSpan tok) "WITHIN GROUP"
                rejectCommonSubexprModifier tok "WITHIN GROUP"
                _ <- expectWordCI "GROUP"
                items <- inParens (expectWordsCI ["ORDER", "BY"] *> parseCommaSeparated parseOrderItem)
                pure (Just (locSpan tok), items)
            else pure (Nothing, [])
    nullTreatment <-
        matchAnyWordCI ["RESPECT", "IGNORE"] >>= traverse \(_, word) -> do
            _ <- expectWordCI "NULLS"
            pure (if asciiEqCI "RESPECT" word then NullTreatmentRespect else NullTreatmentIgnore)
    filterExpr <- whenWordTok "FILTER" \tok -> do
        requireDialectP [PostgreSQL, SQLite, DuckDB, ClickHouse] (locSpan tok) "aggregate FILTER clause"
        rejectCommonSubexprModifier tok "FILTER"
        inParens (expectWordCI "WHERE" *> parseExpr)
    overRef <- whenWordTok "OVER" \tok -> do
        rejectCommonSubexprModifier tok "OVER"
        rejectDisallowedMySQLOver tok
        parseWindowRef
    traverse_
        ( \wSpan -> do
            mariadb <- isDialect MariaDB
            when (mariadb && isNothing overRef) $
                throwParse ParseUnsupported wSpan "MariaDB WITHIN GROUP requires OVER (...)"
        )
        withinTok
    locatedSpanning
        (locSpan name)
        FunctionCall
            { functionName = name
            , functionDistinct = distinct
            , functionArgs = args
            , functionOrderBy = orderBy
            , functionWithinGroup = withinGroup
            , functionFilter = filterExpr
            , functionNullTreatment = nullTreatment
            , functionOver = overRef
            }
    where
        rejectCommonSubexprModifier tok clause = do
            pgOrDuck <- (||) <$> isDialect PostgreSQL <*> isDialect DuckDB
            when (pgOrDuck && isPgCommonSubexpr (locValue name)) $
                throwParse
                    ParseUnsupported
                    (locSpan tok)
                    (clause <> " is not allowed on SQL special value expression")
        isPgCommonSubexpr (Name [L _ (NamePart raw False)]) =
            wordIn ["COALESCE", "GREATEST", "LEAST", "NULLIF"] raw
        isPgCommonSubexpr _ = False
        rejectDisallowedMySQLOver tok = do
            dialect <- gets (dialectId . psProfile)
            when (dialect `elem` [MySQL, MariaDB] && not (isMySQLFamilyOverFunc dialect (locValue name))) $
                throwParse
                    ParseUnsupported
                    (locSpan tok)
                    ( "OVER is only valid on aggregate/window functions in "
                        <> decodeBS (if dialect == MySQL then "mysql" else "mariadb")
                    )
        isMySQLFamilyOverFunc dialect (Name [L _ (NamePart raw False)]) =
            wordIn mysqlWindowAggFuncs raw || (dialect == MariaDB && wordIn mariadbExtraWindowFuncs raw)
        isMySQLFamilyOverFunc _ _ = False
        mysqlWindowAggFuncs =
            [ "AVG"
            , "BIT_AND"
            , "BIT_OR"
            , "BIT_XOR"
            , "COUNT"
            , "CUME_DIST"
            , "DENSE_RANK"
            , "FIRST_VALUE"
            , "GROUP_CONCAT"
            , "JSON_ARRAYAGG"
            , "JSON_OBJECTAGG"
            , "LAG"
            , "LAST_VALUE"
            , "LEAD"
            , "MAX"
            , "MIN"
            , "NTH_VALUE"
            , "NTILE"
            , "PERCENT_RANK"
            , "RANK"
            , "ROW_NUMBER"
            , "STD"
            , "STDDEV"
            , "STDDEV_POP"
            , "STDDEV_SAMP"
            , "SUM"
            , "VAR_POP"
            , "VAR_SAMP"
            , "VARIANCE"
            ]
        mariadbExtraWindowFuncs =
            [ "MEDIAN"
            , "PERCENTILE_CONT"
            , "PERCENTILE_DISC"
            ]

parseFunctionArg :: P (Located FunctionArg)
parseFunctionArg = withLocated do
    mt <- peekTok
    case mt of
        Just (L _ TokStar) -> advanceTok $> FunctionArgStar
        _ -> do
            mNamed <- currentWord
            mColonEq <- peekTokAt 1
            case (mNamed, mColonEq) of
                (Just _, Just (L _ TokColonEq)) -> do
                    namePart <- parseNamePart
                    _ <- expectTok TokColonEq
                    FunctionArgNamed namePart <$> parseExpr
                _ -> FunctionArgExpr <$> parseExpr

-- | Parse a window name or parenthesized window specification.
parseWindowRef :: P WindowRef
parseWindowRef = evalContT do
    onTok TokLParen (WindowRefSpec <$> parseWindowSpec <* expectTok TokRParen)
    lift (WindowRefName <$> parseNamePart)

-- | Parse the body of a window specification, without surrounding parentheses.
parseWindowSpec :: P WindowSpec
parseWindowSpec = do
    mw <- currentHeadWord
    baseName <-
        if maybe False (not . wordIn ["PARTITION", "ORDER", "ROWS", "RANGE", "GROUPS"]) mw
            then Just <$> parseNamePart
            else pure Nothing
    partitionBy <-
        fromMaybe [] <$> whenWord "PARTITION" (expectWordCI "BY" *> parseCommaSeparated parseExpr)
    orderBy <-
        fromMaybe [] <$> whenWord "ORDER" (expectWordCI "BY" *> parseCommaSeparated parseOrderItem)
    frame <-
        matchAnyWordCI ["ROWS", "RANGE", "GROUPS"] >>= traverse \(_, unitWord) -> do
            let unit
                    | asciiEqCI "ROWS" unitWord = FrameRows
                    | asciiEqCI "RANGE" unitWord = FrameRange
                    | otherwise = FrameGroups
            hasBetween <- isJust <$> matchWordCI "BETWEEN"
            startBound <- parseWindowBound
            endBound <- if hasBetween then Just <$> (expectWordCI "AND" *> parseWindowBound) else pure Nothing
            pure (WindowFrame unit startBound endBound)
    pure
        WindowSpec
            { windowBaseName = baseName
            , windowPartitionBy = partitionBy
            , windowOrderBy = orderBy
            , windowFrame = frame
            }

parseWindowBound :: P WindowBound
parseWindowBound = evalContT do
    onWord "UNBOUNDED" $ evalContT do
        onWord "PRECEDING" (pure BoundUnboundedPreceding)
        lift (expectWordCI "FOLLOWING" $> BoundUnboundedFollowing)
    onWord "CURRENT" (expectWordCI "ROW" $> BoundCurrentRow)
    lift do
        expr <- parseExpr
        evalContT do
            onWord "PRECEDING" (pure (BoundPreceding expr))
            lift (expectWordCI "FOLLOWING" $> BoundFollowing expr)

-- | Parse @expr [ASC|DESC] [NULLS FIRST|LAST]@.
parseOrderItem :: P (Located OrderItem)
parseOrderItem = withLocated do
    expr <- parseExpr
    OrderItem expr <$> parseSortDirection <*> parseNullsOrder

-- | Parse an optional @ASC@/@DESC@ direction.
parseSortDirection :: P (Maybe SortDirection)
parseSortDirection = evalContT do
    onWord "ASC" (pure (Just SortAscending))
    onWord "DESC" (pure (Just SortDescending))
    pure Nothing

-- | Parse an optional @NULLS FIRST@/@NULLS LAST@.
parseNullsOrder :: P (Maybe NullsOrder)
parseNullsOrder = evalContT do
    onWordTok "NULLS" \tok -> do
        requireDialectP [PostgreSQL, SQLite, DuckDB, ClickHouse] (locSpan tok) "NULLS FIRST/LAST"
        evalContT do
            onWord "FIRST" (pure (Just NullsFirst))
            lift (expectWordCI "LAST" $> Just NullsLast)
    pure Nothing

-- | Parse @name = expr@ in a SETTINGS list.
parseSettingItem :: P (Located SettingItem)
parseSettingItem = withLocated do
    name <- parseName
    _ <- expectTok TokEq
    SettingItem name <$> parseExpr

-- ---------------------------------------------------------------------------
-- Type names
-- ---------------------------------------------------------------------------

-- | Parse a dialect type name with arguments, suffix words, and array dims.
parseTypeName :: P (Located TypeName)
parseTypeName = withLocated do
    name <- parseName
    argsBefore <- parseOptParenList parseTypeArg
    suffix <- parseTypeSuffix []
    args <- if null argsBefore then parseOptParenList parseTypeArg else pure argsBefore
    dims <- parseArrayDims 0
    pure (TypeName (nameParts (locValue name)) args suffix dims)

parseTypeArg :: P (Located TypeArg)
parseTypeArg = withLocated do
    mt <- peekTok
    mNext <- peekTokAt 1
    case (mt, mNext) of
        (Just (L _ (TokWord wt)), Just (L _ TokEq))
            | isJust (unquotedWord wt) -> TypeArgExpr <$> parseExpr
        (Just (L _ (TokWord _)), _) -> TypeArgType <$> parseTypeName
        _ -> TypeArgExpr <$> parseExpr

parseTypeSuffix :: [Located NamePart] -> P [Located NamePart]
parseTypeSuffix acc = do
    mw <- currentWord
    case mw of
        Just (_, raw)
            | wordIn ["UNSIGNED", "ZEROFILL", "SIGNED", "PRECISION", "VARYING", "ARRAY"] raw ->
                parseNamePart >>= \part -> parseTypeSuffix (part : acc)
            | asciiEqCI "CHARACTER" raw -> do
                isSet <- wordIsAt 1 "SET"
                if isSet
                    then do
                        characterPart <- parseNamePart
                        setPart <- parseNamePart
                        name <- parseName
                        let setParts = setPart : nameParts (locValue name)
                        parseTypeSuffix (reverse setParts ++ (characterPart : acc))
                    else parseNamePart >>= \part -> parseTypeSuffix (part : acc)
            | asciiEqCI "COLLATE" raw -> do
                collatePart <- parseNamePart
                name <- parseName
                parseTypeSuffix (reverse (nameParts (locValue name)) ++ (collatePart : acc))
            | asciiEqCI "WITHOUT" raw || asciiEqCI "WITH" raw -> do
                isTime <- wordIsAt 1 "TIME"
                isZone <- wordIsAt 2 "ZONE"
                if isTime && isZone
                    then do
                        parts <- replicateM 3 parseNamePart
                        parseTypeSuffix (reverse parts ++ acc)
                    else pure (reverse acc)
        _ -> pure (reverse acc)

parseArrayDims :: Int -> P Int
parseArrayDims !n =
    matchTok TokLBracket >>= \case
        Nothing -> pure n
        Just _ -> do
            mt <- peekToken
            when (matchesLitNumber mt) (void advanceTok)
            _ <- expectTok TokRBracket
            parseArrayDims (n + 1)
    where
        matchesLitNumber (Just (TokLiteral LitNumber _)) = True
        matchesLitNumber _ = False

-- ---------------------------------------------------------------------------
-- Index key parts and constraint names
-- ---------------------------------------------------------------------------

-- | Parse an optional constraint or index name when the next token is a word.
parseOptionalConstraintName :: P (Maybe (Located NamePart))
parseOptionalConstraintName =
    peekTok >>= \case
        Just (L _ (TokWord _)) -> Just <$> parseNamePart
        _ -> pure Nothing

-- | Parse a parenthesized index key list.
parseIndexKeyList :: P [Located IndexKeyPart]
parseIndexKeyList = parseParenList parseIndexKeyPart

-- | Parse one index key with optional prefix length, direction, and null order.
parseIndexKeyPart :: P (Located IndexKeyPart)
parseIndexKeyPart = withLocated do
    mt <- peekTok
    (expr, prefix) <- case mt of
        Just (L lpSp TokLParen) -> do
            mariadb <- isDialect MariaDB
            when mariadb $
                throwParse ParseUnsupported lpSp "expression index keys are not supported by MariaDB"
            (,Nothing) <$> parseExpr
        Just (L _ (TokWord _)) -> do
            name <- parseName
            next <- peekToken
            open1 <- peekTokAt 1
            open2 <- peekTokAt 2
            hasPrefixFeature <- hasFeatureP FeatureIndexLengthPrefix
            case (next, open1, open2) of
                (Just TokLParen, Just (L _ (TokLiteral LitNumber raw)), Just (L _ TokRParen))
                    | hasPrefixFeature
                    , length (nameParts (locValue name)) == 1
                    , not (BS.null raw)
                    , BS.all (\b -> b >= 48 && b <= 57) raw -> do
                        prefixExpr <- inParens parseExpr
                        pure (L (locSpan name) (ExprColumn name), Just prefixExpr)
                (Just TokLParen, _, _) -> do
                    when hasPrefixFeature $
                        throwParse
                            ParseUnsupported
                            (locSpan name)
                            "unparenthesized function calls are not valid index key parts in MySQL/MariaDB"
                    call <- parseFunctionCallTail name
                    pure (L (locSpan call) (ExprCall call), Nothing)
                _ -> pure (L (locSpan name) (ExprColumn name), Nothing)
        _ -> (,Nothing) <$> parseExpr
    IndexKeyPart expr prefix <$> parseSortDirection <*> parseNullsOrder
