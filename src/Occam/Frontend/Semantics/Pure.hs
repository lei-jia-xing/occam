-- | Pure built-in predicates, table-function classifiers, and scalar value decoders.
module Occam.Frontend.Semantics.Pure (
    TableFunctionKind (..),
    isPureTableGeneratorName,
    isPureTableGenerator,
    isFileTableFunctionName,
    isFileTableFunction,
    classifyTableFunction,
    extractPathsFromFunctionArgs,
    isNonNullScalarLiteral,
    isUnconditionalCountExpr,
    isEmptyClickHouseTuple,
    isValuesMarker,
    isNiladicSqlFunction,
    indexColumnIdent,
    decodeStringLiteral,
    exprBool,
    exprValue,
    pragmaBool,
    pragmaSessionValue,
    dedupPreserve,
) where

import Control.Monad (guard)
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Foldable qualified as Foldable
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Core.Schema qualified as Core
import Occam.Core.Semantics qualified as Core
import Occam.Frontend.AST.Expr qualified as AST
import Occam.Frontend.AST.Statement qualified as AST
import Occam.Frontend.Context (
    AnalysisContext (..),
    IdentifierClass (..),
    canonicalIdent,
    foldIdentText,
    unquoteLiteral,
    unquoteNamePart,
    unsafeSliceSource,
 )
import Occam.Frontend.Lexer (LiteralKind (..))
import Occam.Frontend.Span (Located (..))
import Occam.Parser.Types (SQLDialect (..))
import Text.Read (readMaybe)

-- | Classification of a table-valued function in @FROM@.
data TableFunctionKind
    = PureGenerator
    | FileTable ![Text]
    | Unmodeled
    deriving (Eq, Show)

-- | Built-in pure row generators that introduce no external catalog dependency.
isPureTableGeneratorName :: Text -> Bool
isPureTableGeneratorName name =
    T.toLower name
        `elem` [ "generate_series"
               , "generate_subscripts"
               , "range"
               , "repeat"
               , "unnest"
               , "numbers"
               , "numbers_mt"
               , "zeros"
               , "zeros_mt"
               ]

builtinTableFunctionIdent :: AST.Name -> Maybe Text
builtinTableFunctionIdent (AST.Name parts) = case parts of
    [L _ part] -> Just (unquoteNamePart part)
    [L _ schema, L _ part]
        | T.toLower (unquoteNamePart schema) == "pg_catalog" -> Just (unquoteNamePart part)
    _ -> Nothing

-- | True when @name@ refers to a built-in pure table generator.
isPureTableGenerator :: AST.Name -> Bool
isPureTableGenerator = maybe False isPureTableGeneratorName . builtinTableFunctionIdent

-- | Built-in file-reading table functions (DuckDB / ClickHouse).
isFileTableFunctionName :: Text -> Bool
isFileTableFunctionName name =
    T.toLower name
        `elem` [ "read_parquet"
               , "read_csv"
               , "read_csv_auto"
               , "read_json"
               , "read_json_auto"
               , "read_ndjson"
               , "read_ndjson_auto"
               , "parquet_scan"
               , "scan_parquet"
               , "glob"
               ]

-- | True when @name@ refers to a file-reading table function.
isFileTableFunction :: AST.Name -> Bool
isFileTableFunction = maybe False isFileTableFunctionName . builtinTableFunctionIdent

-- | Classify a table-valued function call in @FROM@ as pure, file-backed, or unmodeled.
classifyTableFunction :: AST.Name -> [AST.FunctionArg] -> TableFunctionKind
classifyTableFunction name args
    | isPureTableGenerator name = PureGenerator
    | isFileTableFunction name = case extractPathsFromFunctionArgs args of
        [] -> Unmodeled
        paths -> FileTable paths
    | otherwise = Unmodeled

isPathParamName :: Text -> Bool
isPathParamName name = T.toLower name `elem` ["files", "filename", "path", "paths"]

-- | Extract file path literals from positional or named table-function arguments.
extractPathsFromFunctionArgs :: [AST.FunctionArg] -> [Text]
extractPathsFromFunctionArgs args = firstPositional ++ namedPaths
    where
        firstPositional = case args of
            AST.FunctionArgExpr (L _ expr) : _ -> collectStringLiterals expr
            _ -> []
        namedPaths = concatMap collectNamed args
        collectNamed = \case
            AST.FunctionArgNamed (L _ part) (L _ expr)
                | isPathParamName (unquoteNamePart part) -> collectStringLiterals expr
            _ -> []

collectStringLiterals :: AST.Expr -> [Text]
collectStringLiterals = \case
    AST.ExprLiteral lit
        | Just txt <- decodeStringLiteral lit -> [txt]
    AST.ExprArray elems -> concatMap (collectStringLiterals . locValue) elems
    _ -> []

-- | Decode a string-like SQL literal into its unescaped text payload.
decodeStringLiteral :: AST.Literal -> Maybe Text
decodeStringLiteral (AST.Literal kind raw) = case kind of
    LitString -> unquoteSingleOrDouble (TE.decodeUtf8 raw)
    LitNationalString -> unquotePrefixed 'n' (TE.decodeUtf8 raw)
    LitEscapeString -> unquotePrefixed 'e' (TE.decodeUtf8 raw)
    LitDollarString -> unquoteDollar (TE.decodeUtf8 raw)
    _ -> Nothing
    where
        unquoteSingleOrDouble txt = case unquoteLiteral txt of
            Just res -> Just res
            Nothing -> do
                (open, rest) <- T.uncons txt
                (interior, close) <- T.unsnoc rest
                guard (open == close && open `elem` ['\'', '"'])
                Just (T.replace (T.replicate 2 (T.singleton close)) (T.singleton close) interior)
        unquotePrefixed ch txt = do
            (p, rest) <- T.uncons txt
            guard (p == ch || p == toEnum (fromEnum ch - 32))
            unquoteSingleOrDouble rest
        unquoteDollar txt = do
            guard (T.isPrefixOf "$" txt)
            let afterFirst = T.drop 1 txt
                (tag, rest) = T.breakOn "$" afterFirst
            guard (not (T.null rest))
            let delim = "$" <> tag <> "$"
            T.stripPrefix delim txt >>= T.stripSuffix delim

-- | True for non-NULL scalar literals (and unary @+@/@-@ wrapped literals).
isNonNullScalarLiteral :: AST.Expr -> Bool
isNonNullScalarLiteral = \case
    AST.ExprLiteral (AST.Literal kind _) -> kind /= LitBitString
    AST.ExprTypedString {} -> True
    AST.ExprBoolean _ -> True
    AST.ExprUnary AST.UnaryNegate (L _ inner) -> isNonNullScalarLiteral inner
    AST.ExprUnary AST.UnaryPositive (L _ inner) -> isNonNullScalarLiteral inner
    _ -> False

-- | True for an unconditional @COUNT(*)@ or @COUNT(non_null_literal)@ call when unshadowed.
isUnconditionalCountExpr :: AnalysisContext -> AST.Expr -> Bool -> Bool
isUnconditionalCountExpr ctx expr pureTableEnabled
    | not pureTableEnabled = False
    | AST.ExprCall (L _ fn) <- expr
    , Just ident <- countIdent (locValue (AST.functionName fn))
    , matchesName ident "count"
    , null (AST.functionOrderBy fn)
    , null (AST.functionWithinGroup fn)
    , Nothing <- AST.functionFilter fn
    , Nothing <- AST.functionNullTreatment fn
    , Nothing <- AST.functionOver fn
    , not (AST.functionDistinct fn)
    , [L _ arg] <- AST.functionArgs fn =
        case arg of
            AST.FunctionArgStar -> True
            AST.FunctionArgExpr (L _ argExpr) -> isNonNullScalarLiteral argExpr
            _ -> False
    | otherwise = False
    where
        dialect = ctxDialect ctx
        rules = ctxIdentRules ctx
        matchesName part expected =
            canonicalIdent ctx IdentOther part == foldIdentText rules IdentOther False expected
        countIdent (AST.Name parts) = case parts of
            [L _ ident] -> Just ident
            [L _ schema, L _ ident]
                | dialect == PostgreSQL && matchesName schema "pg_catalog" -> Just ident
            _ -> Nothing

-- | True for ClickHouse's zero-argument @tuple()@ placeholder in @ORDER BY@ / @PRIMARY KEY@.
isEmptyClickHouseTuple :: AST.Expr -> Bool
isEmptyClickHouseTuple = \case
    AST.ExprCall (L _ fn)
        | Nothing <- AST.functionOver fn
        , Nothing <- AST.functionFilter fn
        , null (AST.functionWithinGroup fn)
        , Nothing <- AST.functionNullTreatment fn
        , null (AST.functionOrderBy fn)
        , not (AST.functionDistinct fn)
        , null (AST.functionArgs fn)
        , AST.Name [L _ (AST.NamePart raw False)] <- locValue (AST.functionName fn) ->
            T.toLower (TE.decodeUtf8 raw) == "tuple"
    _ -> False

-- | True when @fn@ is the MySQL/MariaDB @VALUES(col)@ proposed-row marker in @ON DUPLICATE KEY UPDATE@.
isValuesMarker :: AST.FunctionCall -> Bool
isValuesMarker fn =
    isNothing (AST.functionOver fn)
        && isNothing (AST.functionFilter fn)
        && null (AST.functionWithinGroup fn)
        && isNothing (AST.functionNullTreatment fn)
        && null (AST.functionOrderBy fn)
        && not (AST.functionDistinct fn)
        && case locValue (AST.functionName fn) of
            AST.Name [L _ (AST.NamePart raw False)] -> T.toLower (TE.decodeUtf8 raw) == "values"
            _ -> False

-- | True for SQL-standard niladic function keywords (e.g. @CURRENT_TIMESTAMP@) parsed without parentheses.
isNiladicSqlFunction :: Text -> Bool
isNiladicSqlFunction name =
    T.toLower name
        `elem` [ "current_timestamp"
               , "current_date"
               , "current_time"
               , "localtime"
               , "localtimestamp"
               , "current_user"
               , "session_user"
               , "current_catalog"
               , "current_role"
               , "current_schema"
               ]

-- | Extract the column identifier of a plain or MySQL prefix-length index key part.
indexColumnIdent :: AnalysisContext -> AST.IndexKeyPart -> Maybe Core.Ident
indexColumnIdent ctx kp = case locValue (AST.indexKeyExpr kp) of
    AST.ExprColumn (L _ (AST.Name [L _ part])) ->
        case AST.indexKeyPrefixLength kp of
            Nothing -> Just (canonicalIdent ctx IdentColumn part)
            Just (L _ (AST.ExprLiteral (AST.Literal LitNumber raw)))
                | ctxDialect ctx `elem` [MySQL, MariaDB]
                , not (BS.null raw)
                , BS.all (\b -> b >= 48 && b <= 57) raw ->
                    Just (canonicalIdent ctx IdentColumn part)
            _ -> Nothing
    _ -> Nothing

-- | Parse a boolean literal or keyword identifier (@ON@/@OFF@, @TRUE@/@FALSE@, @1@/@0@).
exprBool :: AST.Expr -> Maybe Bool
exprBool = \case
    AST.ExprBoolean b -> Just b
    AST.ExprLiteral (AST.Literal LitNumber raw) ->
        (/= (0 :: Integer)) <$> readMaybe (T.unpack (TE.decodeUtf8 raw))
    AST.ExprLiteral lit
        | Just txt <- decodeStringLiteral lit ->
            case T.toLower txt of
                "on" -> Just True
                "true" -> Just True
                "1" -> Just True
                "off" -> Just False
                "false" -> Just False
                "0" -> Just False
                _ -> Nothing
    AST.ExprBareKeyword (L _ raw) -> keywordBool raw
    AST.ExprColumn (L _ (AST.Name [L _ (AST.NamePart raw _)])) -> keywordBool raw
    _ -> Nothing
    where
        keywordBool raw = case T.toLower (TE.decodeUtf8 raw) of
            "on" -> Just True
            "true" -> Just True
            "off" -> Just False
            "false" -> Just False
            _ -> Nothing

-- | Evaluate a session variable assignment expression into a 'Core.SessionValue'.
exprValue :: AnalysisContext -> AST.Expr -> Core.SessionValue
exprValue ctx = \case
    AST.ExprBoolean b -> Core.SessionBool b
    AST.ExprNull -> Core.SessionText "NULL"
    AST.ExprDefault -> Core.SessionDefault
    AST.ExprLiteral (AST.Literal LitNumber raw) ->
        let txt = TE.decodeUtf8 raw
         in maybe (Core.SessionText txt) Core.SessionInteger (readMaybe (T.unpack txt))
    AST.ExprLiteral lit ->
        maybe
            (Core.SessionText (TE.decodeUtf8 (AST.literalRaw lit)))
            Core.SessionText
            (decodeStringLiteral lit)
    AST.ExprUnary AST.UnaryNegate (L _ (AST.ExprLiteral (AST.Literal LitNumber raw))) ->
        let txt = "-" <> TE.decodeUtf8 raw
         in maybe (Core.SessionText txt) Core.SessionInteger (readMaybe (T.unpack txt))
    AST.ExprUnary AST.UnaryPositive (L _ (AST.ExprLiteral (AST.Literal LitNumber raw))) ->
        let txt = TE.decodeUtf8 raw
         in maybe (Core.SessionText txt) Core.SessionInteger (readMaybe (T.unpack txt))
    AST.ExprBareKeyword (L _ raw) -> identOrKeyword (TE.decodeUtf8 raw)
    AST.ExprColumn (L _ (AST.Name [L _ (AST.NamePart raw False)])) -> identOrKeyword (TE.decodeUtf8 raw)
    AST.ExprColumn (L _ (AST.Name [L _ part@(AST.NamePart raw True)]))
        | ctxDialect ctx `elem` [MySQL, MariaDB] && BS.isPrefixOf "\"" raw ->
            Core.SessionText (unquoteNamePart part)
        | otherwise -> identOrKeyword (unquoteNamePart part)
    AST.ExprColumn (L sp _) -> Core.SessionText (TE.decodeUtf8 (unsafeSliceSource ctx sp))
    AST.ExprField (L sp1 _) (L sp2 _) ->
        let sp = sp1 <> sp2
         in Core.SessionText (TE.decodeUtf8 (unsafeSliceSource ctx sp))
    other -> Core.SessionText (renderExprFallback ctx other)
    where
        identOrKeyword raw = case T.toLower raw of
            "default" -> Core.SessionDefault
            "on" -> Core.SessionBool True
            "true" -> Core.SessionBool True
            "off" -> Core.SessionBool False
            "false" -> Core.SessionBool False
            _ -> Core.SessionText raw

renderExprFallback :: AnalysisContext -> AST.Expr -> Text
renderExprFallback ctx = \case
    AST.ExprUnary AST.UnaryNegate (L _ inner) -> "-" <> exprText inner
    AST.ExprUnary AST.UnaryPositive (L _ inner) -> "+" <> exprText inner
    AST.ExprBinary op (L sp1 e1) (L sp2 e2) ->
        let s1 = TE.decodeUtf8 (unsafeSliceSource ctx sp1)
            s2 = TE.decodeUtf8 (unsafeSliceSource ctx sp2)
         in if T.null s1 || T.null s2
                then exprText e1 <> " " <> binaryOpText op <> " " <> exprText e2
                else s1 <> " " <> binaryOpText op <> " " <> s2
    AST.ExprParen (L sp _) -> "(" <> TE.decodeUtf8 (unsafeSliceSource ctx sp) <> ")"
    AST.ExprParam _ raw -> TE.decodeUtf8 raw
    AST.ExprCall (L sp _) -> TE.decodeUtf8 (unsafeSliceSource ctx sp)
    _ -> ""
    where
        exprText e = case exprValue ctx e of
            Core.SessionText t -> t
            Core.SessionBool True -> "true"
            Core.SessionBool False -> "false"
            Core.SessionInteger n -> T.pack (show n)
            Core.SessionDefault -> "DEFAULT"
            Core.SessionNames ns -> T.intercalate "." (map Core.nameText ns)
        binaryOpText = \case
            AST.OpAdd -> "+"
            AST.OpSub -> "-"
            AST.OpMul -> "*"
            AST.OpDiv -> "/"
            AST.OpEq -> "="
            _ -> ""

-- | Parse a boolean setting from a @PRAGMA@ assignment value.
pragmaBool :: AST.PragmaValue -> Maybe Bool
pragmaBool assignment = case pragmaSessionValue assignment of
    Just (Core.SessionText value) -> boolSpelling (T.toLower value)
    Just (Core.SessionBool value) -> Just value
    Just (Core.SessionInteger n) -> Just (n /= 0)
    _ -> Nothing
    where
        boolSpelling raw
            | raw `elem` ["true", "yes", "on", "1"] = Just True
            | raw `elem` ["false", "no", "off", "0"] = Just False
            | otherwise = Nothing

-- | Decode a @PRAGMA@ assignment value into a 'Core.SessionValue'.
pragmaSessionValue :: AST.PragmaValue -> Maybe Core.SessionValue
pragmaSessionValue (AST.PragmaBare (L _ raw)) =
    let txt = TE.decodeUtf8 raw
     in case T.toLower txt of
            "true" -> Just (Core.SessionBool True)
            "false" -> Just (Core.SessionBool False)
            _ -> Just (Core.SessionText txt)
pragmaSessionValue (AST.PragmaValueExpr (L _ expr)) = case expr of
    AST.ExprBoolean b -> Just (Core.SessionBool b)
    AST.ExprLiteral (AST.Literal LitNumber raw) -> sessionInteger (TE.decodeUtf8 raw)
    AST.ExprLiteral lit -> Core.SessionText <$> decodeStringLiteral lit
    AST.ExprColumn (L _ (AST.Name [L _ (AST.NamePart raw True)])) ->
        Core.SessionText <$> unquoteLiteral (TE.decodeUtf8 raw)
    AST.ExprUnary AST.UnaryNegate (L _ (AST.ExprLiteral (AST.Literal LitNumber raw))) ->
        sessionInteger ("-" <> TE.decodeUtf8 raw)
    AST.ExprUnary AST.UnaryPositive (L _ (AST.ExprLiteral (AST.Literal LitNumber raw))) ->
        sessionInteger (TE.decodeUtf8 raw)
    _ -> Nothing
    where
        sessionInteger value
            | T.all isDigit (T.dropWhile (== '-') value) = Core.SessionInteger <$> readMaybe (T.unpack value)
            | otherwise = Nothing

-- | Deduplicate a list while preserving the first-seen element order.
dedupPreserve :: (Ord a) => [a] -> [a]
dedupPreserve = reverse . fst . Foldable.foldl' step ([], Set.empty)
    where
        step (acc, seen) x
            | Set.member x seen = (acc, seen)
            | otherwise = (x : acc, Set.insert x seen)
