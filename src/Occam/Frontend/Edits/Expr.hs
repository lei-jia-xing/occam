-- | Expression, window-specification, and literal edit generation.
module Occam.Frontend.Edits.Expr (
    exprSpan,
    sliceExpr,
    functionArgSpan,
    scalarLiteralSpan,
    addScalarEdit,
    visitExpr,
    visitWindowSpec,
) where

import Control.Monad (forM_, guard, join, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Vector qualified as V
import Occam.Facts (ClauseTier (..), EditTier (..), ExprTier (..), ParserEditCategory (..))
import Occam.Frontend.AST qualified as AST
import Occam.Frontend.Context (AnalysisContext (..))
import Occam.Frontend.Edits.Core (
    EditM,
    absTuple,
    addClauseEdit,
    addEditBytesWithFlags,
    addExprEdit,
    addLowYieldExprEdit,
    addTypeEdits,
    askCtx,
    askSource,
    askTokens,
    listEdits,
    sliceAbs,
 )
import Occam.Frontend.Edits.Tokens (
    TokenIndex (..),
    duplicateTreatmentRange,
    isWordCI,
    nextSignificant,
    partitionPoint,
    precedingSpace,
    topLevelKeyword,
 )
import Occam.Frontend.Lexer (LiteralKind (..), Token (..), asciiEqCI)
import Occam.Frontend.Span (ByteSpan (..), Located (..))
import Occam.Parser.Types (SQLDialect (..))

-- | Compute the source byte range @(start, end)@ for expressions supported by Rust's @expression_source_span@.
exprSpan :: AnalysisContext -> TokenIndex -> Located AST.Expr -> Maybe (Int, Int)
exprSpan ctx ti (L sp node) = case node of
    AST.ExprLiteral _ -> Just absSp
    AST.ExprTypedString {} -> Just absSp
    AST.ExprNull -> Just absSp
    AST.ExprBoolean _ -> Just absSp
    AST.ExprParam AST.ParamPositional _ -> Just absSp
    AST.ExprParam AST.ParamUser _ -> Just absSp
    AST.ExprParam AST.ParamSystem _ -> Just absSp
    AST.ExprColumn _ -> Just absSp
    AST.ExprStar -> Just absSp
    AST.ExprDefault -> Just absSp
    AST.ExprBinary _ left right -> do
        (lStart, _) <- exprSpan ctx ti left
        (_, rEnd) <- exprSpan ctx ti right
        pure (lStart, rEnd)
    AST.ExprUnary AST.UnaryNot (L _ (AST.ExprExists _)) -> Just absSp
    AST.ExprUnary _ inner -> do
        (_, innerEnd) <- exprSpan ctx ti inner
        pure (fst absSp, innerEnd)
    AST.ExprParen inner -> exprSpan ctx ti inner >> Just absSp
    AST.ExprSubquery _ -> Just absSp
    AST.ExprExists _ -> Just absSp
    AST.ExprIsNull _ inner -> exprSpan ctx ti inner >> Just absSp
    AST.ExprIsTrue _ inner -> exprSpan ctx ti inner >> Just absSp
    AST.ExprIsFalse _ inner -> exprSpan ctx ti inner >> Just absSp
    AST.ExprIsUnknown _ inner -> exprSpan ctx ti inner >> Just absSp
    AST.ExprIsDistinct _ left right -> do
        _ <- exprSpan ctx ti left
        _ <- exprSpan ctx ti right
        Just absSp
    AST.ExprBetween _ inner low high -> do
        _ <- exprSpan ctx ti inner
        _ <- exprSpan ctx ti low
        _ <- exprSpan ctx ti high
        Just absSp
    AST.ExprLike _ _ inner pat mEsc -> do
        _ <- exprSpan ctx ti inner
        _ <- exprSpan ctx ti pat
        traverse_ (exprSpan ctx ti) mEsc
        Just absSp
    AST.ExprCollate inner _ -> exprSpan ctx ti inner >> Just absSp
    AST.ExprInList _ lhs items -> do
        _ <- exprSpan ctx ti lhs
        case reverse items of
            lastItem : _ -> exprSpan ctx ti lastItem >> Just absSp
            [] -> Nothing
    AST.ExprInSubquery _ lhs _ -> exprSpan ctx ti lhs >> Just absSp
    AST.ExprCast style inner _
        | style == AST.CastDotColon -> Nothing
        | otherwise -> exprSpan ctx ti inner >> Just absSp
    AST.ExprCastExpr inner tyExpr -> do
        _ <- exprSpan ctx ti inner
        _ <- exprSpan ctx ti tyExpr
        Just absSp
    AST.ExprCase _ -> Just absSp
    AST.ExprTuple items -> case (items, reverse items) of
        (firstItem : _, lastItem : _) -> do
            _ <- exprSpan ctx ti firstItem
            _ <- exprSpan ctx ti lastItem
            Just absSp
        _ -> Nothing
    AST.ExprCall (L _ fc)
        | isJust (AST.functionNullTreatment fc)
            || not (null (AST.functionWithinGroup fc))
            || isJust (AST.functionFilter fc) ->
            Nothing
        | otherwise -> Just absSp
    _ -> Nothing
    where
        !absSp = absTuple ctx sp

-- | Slice an expression's source bytes when 'exprSpan' resolves.
sliceExpr :: AnalysisContext -> TokenIndex -> Located AST.Expr -> Maybe ByteString
sliceExpr ctx ti expr = exprSpan ctx ti expr >>= sliceAbs (ctxSource ctx)

-- | Source byte range for a function argument (excluding bare @*@).
functionArgSpan :: AnalysisContext -> TokenIndex -> Located AST.FunctionArg -> Maybe (Int, Int)
functionArgSpan ctx ti (L _ arg) = case arg of
    AST.FunctionArgExpr (L qSp (AST.ExprQualifiedStar _)) -> Just (absTuple ctx qSp)
    AST.FunctionArgExpr e -> exprSpan ctx ti e
    AST.FunctionArgStar -> Nothing
    AST.FunctionArgNamed namePart e -> do
        (_, eEnd) <- exprSpan ctx ti e
        let (nStart, _) = absTuple ctx (locSpan namePart)
        pure (nStart, eEnd)

-- | Span of the scalar literal if @query@ is a bare @SELECT <literal>@ with no FROM or WHERE.
scalarLiteralSpan :: AST.Query -> Maybe ByteSpan
scalarLiteralSpan q = case locValue (AST.queryBody q) of
    AST.QueryBodySelect core
        | null (AST.selectFrom core)
        , Nothing <- AST.selectWhere core
        , [L _ item] <- AST.selectItems core
        , Nothing <- AST.selectItemAlias item
        , not (AST.selectItemAsWritten item) ->
            let L litSp exprVal = AST.selectItemExpr item
             in case exprVal of
                    AST.ExprLiteral _ -> Just litSp
                    AST.ExprNull -> Just litSp
                    AST.ExprBoolean _ -> Just litSp
                    AST.ExprParam AST.ParamPositional _ -> Just litSp
                    _ -> Nothing
    _ -> Nothing

-- | Emit literal simplification edits strictly shorter than @sp@.
addScalarEdit :: ByteSpan -> Bool -> EditM ()
addScalarEdit sp isString = do
    ctx <- askCtx
    let (start, end) = absTuple ctx sp
        !len = end - start
        replacements =
            if isString
                then [("''", False), ("'0'", True), ("NULL", True), ("0", True)]
                else [("0", False), ("1", False), ("NULL", True), ("FALSE", True), ("TRUE", True), ("''", True)]
    forM_ replacements \(repl, lowYield) ->
        when (BS.length repl < len) $
            addEditBytesWithFlags start end repl EditLiteral TierDefault "simplify-literal" isString lowYield

isStringLiteral :: AST.Literal -> Bool
isStringLiteral lit =
    AST.literalKind lit == LitString || BS.isPrefixOf "'" (AST.literalRaw lit)

-- | Emit window specification clause and list edits for an inline @OVER (...)@ window.
windowSpecEdits :: AST.WindowSpec -> Int -> EditM ()
windowSpecEdits spec after = do
    ctx <- askCtx
    src <- askSource
    ti <- askTokens
    let ByteSpan _ unitEnd = ctxUnitSpan ctx
        toks = tiTokens ti
        !afterIdx = partitionPoint (\t -> spanStart (locSpan t) < after) toks
    forM_ (findOverParen ti afterIdx unitEnd) \pair -> do
        let !insideStart = spanStart pair + 1
            !insideEnd = spanEnd pair - 1
            !scope = (insideStart, insideEnd)
        when
            ( not (null (AST.windowPartitionBy spec))
                || not (null (AST.windowOrderBy spec))
                || isJust (AST.windowFrame spec)
                || isJust (AST.windowBaseName spec)
            )
            (addClauseEdit SafeClause insideStart insideEnd "" "simplify-window_spec_empty")
        let frameKw = fmap (frameUnitKw . AST.windowFrameUnit) (AST.windowFrame spec)
            rawClauses =
                [
                    ( if null (AST.windowPartitionBy spec) then Nothing else Just "PARTITION"
                    , "remove-window_partition_by"
                    )
                , (if null (AST.windowOrderBy spec) then Nothing else Just "ORDER", "remove-window_order_by")
                , (frameKw, "remove-window_frame")
                ]
            resolveClause (Nothing, _) = Just Nothing
            resolveClause (Just kw, lbl) = case topLevelKeyword ti kw scope of
                Just tok -> Just (Just (spanStart (locSpan tok), lbl))
                Nothing -> Nothing
        case traverse resolveClause rawClauses of
            Nothing -> pure ()
            Just maybeClauses ->
                let clauses = catMaybes maybeClauses
                 in when (strictlyIncreasing (map fst clauses)) $
                        emitWindowClauses src insideStart insideEnd clauses
        when (length (AST.windowPartitionBy spec) > 1) $
            listEdits
                "remove-window_partition_by_expression"
                (AST.windowPartitionBy spec)
                (pure . exprSpan ctx ti)
        when (length (AST.windowOrderBy spec) > 1) $
            listEdits
                "remove-window_order_by_expression"
                (AST.windowOrderBy spec)
                \(L _ item) ->
                    pure $
                        if isNothing (AST.orderDirection item) && isNothing (AST.orderNulls item)
                            then exprSpan ctx ti (AST.orderExpr item)
                            else Nothing
    where
        findOverParen ti afterIdx unitEnd = do
            (overIdx, overTok) <- nextSignificant ti afterIdx unitEnd
            guard (isWordCI "OVER" overTok)
            (parenIdx, L _ TokLParen) <- nextSignificant ti (overIdx + 1) unitEnd
            join (tiParens ti V.!? parenIdx)

        frameUnitKw = \case
            AST.FrameRows -> "ROWS"
            AST.FrameRange -> "RANGE"
            AST.FrameGroups -> "GROUPS"

        strictlyIncreasing [] = True
        strictlyIncreasing [_] = True
        strictlyIncreasing (x : y : xs) = x < y && strictlyIncreasing (y : xs)

        emitWindowClauses _ _ _ [] = pure ()
        emitWindowClauses src insideStart insideEnd [(s, lbl)] =
            let !s' = max insideStart (precedingSpace src s)
             in addClauseEdit SafeClause s' insideEnd "" lbl
        emitWindowClauses src insideStart insideEnd ((s, lbl) : rest@((nextS, _) : _)) = do
            addClauseEdit SafeClause s nextS "" lbl
            emitWindowClauses src insideStart insideEnd rest

-- | Traverse an expression in Rust @sqlparser@ AST order, emitting expression and literal edits.
visitExpr :: (Located AST.Query -> EditM ()) -> Located AST.Expr -> EditM ()
visitExpr visitQuery expr@(L sp node) = do
    ctx <- askCtx
    ti <- askTokens
    let src = ctxSource ctx
    case node of
        AST.ExprUnary AST.UnaryNot (L _ (AST.ExprExists q)) -> do
            forM_ (exprSpan ctx ti expr) \(start, end) ->
                addExprEdit AggressiveExpr start end "0" "simplify-exists"
            visitQuery q
        AST.ExprBinary _ left right -> do
            forM_ (exprSpan ctx ti expr) \(start, end) -> do
                forM_ [left, right] \cand ->
                    forM_ (sliceExpr ctx ti cand) \repl ->
                        addExprEdit UnwrapExpr start end repl "simplify-binary_expression"
                forM_ ["0", "1", "'0'"] \repl ->
                    addLowYieldExprEdit start end repl "simplify-binary_expression"
            visitExpr visitQuery left
            visitExpr visitQuery right
        AST.ExprUnary _ operand -> do
            forM_ (exprSpan ctx ti expr) \(start, end) -> do
                forM_ (sliceExpr ctx ti operand) \repl ->
                    addExprEdit UnwrapExpr start end repl "simplify-unary_expression"
                forM_ ["0", "1", "'0'"] \repl ->
                    addLowYieldExprEdit start end repl "simplify-unary_expression"
            visitExpr visitQuery operand
        AST.ExprParen inner -> do
            forM_ (exprSpan ctx ti expr) \(start, end) ->
                forM_ (sliceExpr ctx ti inner) \repl ->
                    addExprEdit UnwrapExpr start end repl "simplify-parenthesized_expression"
            visitExpr visitQuery inner
        AST.ExprExists q -> do
            forM_ (exprSpan ctx ti expr) \(start, end) ->
                addExprEdit AggressiveExpr start end "0" "simplify-exists"
            visitQuery q
        AST.ExprSubquery q -> do
            forM_ (scalarLiteralSpan (locValue q)) \litSp ->
                forM_ (exprSpan ctx ti expr) \(start, end) ->
                    forM_ (sliceAbs src (absTuple ctx litSp)) \repl ->
                        addExprEdit UnwrapExpr start end repl "simplify-scalar-subquery"
            visitQuery q
        AST.ExprInList _ lhs items -> do
            listEdits "remove-in_list" items (pure . exprSpan ctx ti)
            forM_ (exprSpan ctx ti expr) \(start, end) -> do
                forM_ (sliceExpr ctx ti lhs) \repl ->
                    addExprEdit UnwrapExpr start end repl "simplify-in_list"
                forM_ ["1", "0"] \repl ->
                    addLowYieldExprEdit start end repl "simplify-in_list"
            visitExpr visitQuery lhs
            mapM_ (visitExpr visitQuery) items
        AST.ExprInSubquery _ lhs q -> do
            forM_ (exprSpan ctx ti expr) \(start, end) -> do
                forM_ (sliceExpr ctx ti lhs) \repl ->
                    addExprEdit UnwrapExpr start end repl "simplify-in_subquery"
                forM_ ["1", "0"] \repl ->
                    addLowYieldExprEdit start end repl "simplify-in_subquery"
            visitExpr visitQuery lhs
            visitQuery q
        AST.ExprBetween _ inner low high -> do
            forM_ (exprSpan ctx ti expr) \(start, end) -> do
                forM_ [inner, low, high] \cand ->
                    forM_ (sliceExpr ctx ti cand) \repl ->
                        addExprEdit UnwrapExpr start end repl "simplify-between"
                forM_ ["1", "0"] \repl ->
                    addLowYieldExprEdit start end repl "simplify-between"
            visitExpr visitQuery inner
            visitExpr visitQuery low
            visitExpr visitQuery high
        AST.ExprLike _ likeOp inner pat mEsc -> do
            when (likeOp == AST.LikeLike || likeOp == AST.LikeILike) $
                forM_ (exprSpan ctx ti expr) \(start, end) -> do
                    forM_ (sliceExpr ctx ti inner) \repl ->
                        addExprEdit UnwrapExpr start end repl "simplify-like"
                    forM_ ["1", "0"] \repl ->
                        addLowYieldExprEdit start end repl "simplify-like"
            visitExpr visitQuery inner
            visitExpr visitQuery pat
            traverse_ (visitExpr visitQuery) mEsc
        AST.ExprIsNull _ inner -> do
            forM_ (exprSpan ctx ti expr) \(start, end) -> do
                forM_ (sliceExpr ctx ti inner) \repl ->
                    addExprEdit UnwrapExpr start end repl "simplify-is_null"
                forM_ ["0", "1"] \repl ->
                    addLowYieldExprEdit start end repl "simplify-is_null"
            visitExpr visitQuery inner
        AST.ExprCase (AST.CaseExpr mOp whens mElse) -> do
            forM_ (exprSpan ctx ti expr) \(start, end) -> do
                forM_ whens \(_, res) ->
                    forM_ (sliceExpr ctx ti res) \repl ->
                        addExprEdit UnwrapExpr start end repl "simplify-case_result"
                forM_ mElse \elseExpr ->
                    forM_ (sliceExpr ctx ti elseExpr) \repl ->
                        addExprEdit UnwrapExpr start end repl "simplify-case_else"
                forM_ ["0", "1"] \repl ->
                    addExprEdit AggressiveExpr start end repl "simplify-case_literal"
            traverse_ (visitExpr visitQuery) mOp
            forM_ whens \(cond, res) -> do
                visitExpr visitQuery cond
                visitExpr visitQuery res
            traverse_ (visitExpr visitQuery) mElse
        AST.ExprTuple items -> do
            listEdits "remove-tuple_item" items (pure . exprSpan ctx ti)
            mapM_ (visitExpr visitQuery) items
        AST.ExprCall (L callSp fc) -> do
            let ByteSpan _ unitEnd = ctxUnitSpan ctx
                (_, nameEnd) = absTuple ctx (locSpan (AST.functionName fc))
                (_, callEnd) = absTuple ctx callSp
            forM_ (duplicateTreatmentRange ti nameEnd unitEnd) \(kwStart, nextStart) ->
                addClauseEdit SafeClause kwStart nextStart "" "remove-distinct"
            listEdits "remove-function_argument" (AST.functionArgs fc) (pure . functionArgSpan ctx ti)
            forM_ (exprSpan ctx ti expr) \(start, end) ->
                forM_ (AST.functionArgs fc) \arg ->
                    forM_ (functionArgSpan ctx ti arg) \aSpan ->
                        forM_ (sliceAbs src aSpan) \repl ->
                            addExprEdit UnwrapExpr start end repl "simplify-function_to_argument"
            case AST.functionOver fc of
                Just (AST.WindowRefSpec spec) ->
                    let !afterNameIdx = partitionPoint (\t -> spanStart (locSpan t) < nameEnd) (tiTokens ti)
                        argsEnd = case nextSignificant ti afterNameIdx callEnd of
                            Just (pIdx, L _ TokLParen) -> case tiParens ti V.!? pIdx of
                                Just (Just pair) -> spanEnd pair
                                _ -> nameEnd
                            _ -> nameEnd
                        overAnchor = case topLevelKeyword ti "OVER" (argsEnd, callEnd) of
                            Just overTok -> spanStart (locSpan overTok)
                            Nothing -> argsEnd
                     in windowSpecEdits spec overAnchor
                _ -> pure ()
            forM_ (exprSpan ctx ti expr) \(start, end) ->
                forM_ ["0", "1"] \repl ->
                    addExprEdit AggressiveExpr start end repl "simplify-function_literal"
            forM_ (AST.functionArgs fc) \(L _ arg) -> case arg of
                AST.FunctionArgExpr e -> visitExpr visitQuery e
                AST.FunctionArgNamed _ e -> visitExpr visitQuery e
                AST.FunctionArgStar -> pure ()
            forM_ (AST.functionOrderBy fc) (visitExpr visitQuery . AST.orderExpr . locValue)
            forM_ (AST.functionWithinGroup fc) (visitExpr visitQuery . AST.orderExpr . locValue)
            traverse_ (visitExpr visitQuery) (AST.functionFilter fc)
            forM_ (AST.functionOver fc) \case
                AST.WindowRefSpec spec -> visitWindowSpec visitQuery spec
                AST.WindowRefName _ -> pure ()
        AST.ExprCast style inner ty -> do
            when (style /= AST.CastDotColon) $
                forM_ (exprSpan ctx ti expr) \(start, end) -> do
                    addTypeEdits (absTuple ctx (locSpan ty))
                    forM_ (sliceExpr ctx ti inner) \repl -> do
                        addExprEdit UnwrapExpr start end repl "simplify-cast"
                        forM_ ["0", "1"] \lit ->
                            addExprEdit AggressiveExpr start end lit "simplify-cast_literal"
            visitExpr visitQuery inner
        AST.ExprCastExpr inner tyExpr -> do
            listEdits "remove-function_argument" [inner, tyExpr] (pure . exprSpan ctx ti)
            forM_ (exprSpan ctx ti expr) \(start, end) -> do
                forM_ [inner, tyExpr] \arg ->
                    forM_ (sliceExpr ctx ti arg) \repl ->
                        addExprEdit UnwrapExpr start end repl "simplify-function_to_argument"
                forM_ ["0", "1"] \repl ->
                    addExprEdit AggressiveExpr start end repl "simplify-function_literal"
            visitExpr visitQuery inner
            visitExpr visitQuery tyExpr
        AST.ExprLiteral lit -> addScalarEdit sp (isStringLiteral lit)
        AST.ExprNull -> addScalarEdit sp False
        AST.ExprBoolean _ -> addScalarEdit sp False
        AST.ExprParam AST.ParamPositional _ -> addScalarEdit sp False
        AST.ExprCharsetString _ (L litSp lit) -> addScalarEdit litSp (isStringLiteral lit)
        AST.ExprTypedString {} -> pure ()
        AST.ExprLambda _ body -> visitExpr visitQuery body
        AST.ExprSubscript base lo hi -> do
            visitExpr visitQuery base
            traverse_ (visitExpr visitQuery) lo
            traverse_ (visitExpr visitQuery) hi
        AST.ExprField base _ -> visitExpr visitQuery base
        AST.ExprFieldIndex base _ -> visitExpr visitQuery base
        AST.ExprFieldStar base -> visitExpr visitQuery base
        AST.ExprIsTrue _ inner -> visitExpr visitQuery inner
        AST.ExprIsFalse _ inner -> visitExpr visitQuery inner
        AST.ExprIsUnknown _ inner -> visitExpr visitQuery inner
        AST.ExprIsDistinct _ left right -> do
            visitExpr visitQuery left
            visitExpr visitQuery right
        AST.ExprQuantified _ _ left right -> do
            visitExpr visitQuery left
            visitExpr visitQuery right
        AST.ExprArray items -> mapM_ (visitExpr visitQuery) items
        AST.ExprStruct fields -> forM_ fields \(_, v) ->
            visitExpr visitQuery v
        AST.ExprCollate inner _ -> visitExpr visitQuery inner
        AST.ExprColumn (L _ (AST.Name [L _ (AST.NamePart raw False)]))
            | isNiladicFunction (ctxDialect ctx) raw ->
                let (start, end) = absTuple ctx sp
                 in forM_ ["0", "1"] \repl ->
                        addExprEdit AggressiveExpr start end repl "simplify-function_literal"
        AST.ExprColumn (L _ (AST.Name [L partSp (AST.NamePart _ True)]))
            | ctxDialect ctx `elem` [MySQL, MariaDB]
            , Just rawSlice <- sliceAbs src (absTuple ctx partSp)
            , BS.isPrefixOf "\"" rawSlice ->
                addScalarEdit sp False
        AST.ExprColumn _ -> pure ()
        AST.ExprStar -> pure ()
        AST.ExprQualifiedStar _ -> pure ()
        AST.ExprParam AST.ParamUser _ -> pure ()
        AST.ExprParam AST.ParamSystem _ -> pure ()
        AST.ExprDefault -> pure ()
        AST.ExprBareKeyword _ -> pure ()

-- | Traverse expressions inside a 'AST.WindowSpec'.
visitWindowSpec :: (Located AST.Query -> EditM ()) -> AST.WindowSpec -> EditM ()
visitWindowSpec visitQuery spec = do
    mapM_ (visitExpr visitQuery) (AST.windowPartitionBy spec)
    forM_ (AST.windowOrderBy spec) (visitExpr visitQuery . AST.orderExpr . locValue)
    traverse_ visitFrame (AST.windowFrame spec)
    where
        visitFrame frame = do
            visitBound (AST.windowFrameStart frame)
            traverse_ visitBound (AST.windowFrameEnd frame)
        visitBound = \case
            AST.BoundPreceding e -> visitExpr visitQuery e
            AST.BoundFollowing e -> visitExpr visitQuery e
            _ -> pure ()

isNiladicFunction :: SQLDialect -> ByteString -> Bool
isNiladicFunction dialect raw =
    any (`asciiEqCI` raw) commonNiladic
        || (dialect == PostgreSQL && any (`asciiEqCI` raw) pgNiladic)
    where
        commonNiladic =
            [ "CURRENT_TIMESTAMP"
            , "CURRENT_TIME"
            , "CURRENT_DATE"
            , "LOCALTIME"
            , "LOCALTIMESTAMP"
            ]
        pgNiladic =
            [ "CURRENT_CATALOG"
            , "CURRENT_USER"
            , "SESSION_USER"
            , "USER"
            ]
