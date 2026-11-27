{- | Structural span and raw-byte invariants for parsed frontend statements.

The walk is driven by the derived 'Data' instances of the AST.  It treats
'Located' and 'OpaqueBody' as span-bearing nodes, checks every such span for
UTF-8 validity and nearest-parent containment, and checks ordered
non-overlap only for lists (the AST's source-ordered data).  A small explicit
registry covers the raw-byte leaves whose spelling is contractual; other
located payloads are deliberately constructed and are not compared to source.
-}
module FrontendAstInvariants (
    invariantViolations,
    validateStatementInvariants,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Data (
    Data,
    constrIndex,
    dataTypeName,
    dataTypeOf,
    gmapQ,
    showConstr,
    toConstr,
 )
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable, cast)
import Data.Word (Word8)
import Occam.Frontend.AST (Expr (ExprLiteral), Literal (..), NamePart (..), Statement)
import Occam.Frontend.Lexer (asciiUpperBS)
import Occam.Frontend.Span (
    ByteSpan (..),
    Located (..),
    isNonEmptyValidSpan,
    sliceSpan,
    spanEnd,
    spanStart,
 )
import Test.Tasty.HUnit (Assertion, assertFailure)

data Box = forall a. (Data a) => Box a

data Tag = Tag
    { tagType :: !String
    , tagConstructor :: !String
    }

data Ctx = Ctx
    { ctxParent :: !(Maybe ByteSpan)
    , ctxTags :: ![Tag]
    }

-- | All invariant failures for one successful parse, in traversal order.
invariantViolations :: ByteString -> Located Statement -> [String]
invariantViolations sql = walkValue sql (Ctx Nothing [])

validateStatementInvariants :: ByteString -> Located Statement -> Assertion
validateStatementInvariants sql statement =
    case invariantViolations sql statement of
        [] -> pure ()
        violations ->
            assertFailure
                ( "statement AST invariant failures ("
                    <> show (length violations)
                    <> "):\n"
                    <> intercalate "\n" (take 40 violations)
                    <> if length violations > 40 then "\n..." else ""
                )

-- ---------------------------------------------------------------------------
-- Generic walk
-- ---------------------------------------------------------------------------

walkValue :: forall a. (Data a) => ByteString -> Ctx -> a -> [String]
walkValue sql ctx value
    | isLocatedNode value = walkLocated sql ctx value
    | isOpaqueNode value = walkOpaque sql ctx value
    | isListType value = walkList sql ctx value
    | isLeafType value = []
    | otherwise = walkChildren sql ctx value

walkLocated :: forall a. (Data a) => ByteString -> Ctx -> a -> [String]
walkLocated sql ctx value =
    case firstChildAs value of
        Nothing -> ["Located node without a ByteSpan"]
        Just sp ->
            spanIssues sql "Located" sp
                ++ containmentIssues "Located" sp (ctxParent ctx)
                ++ rawIssues sql (ctxTags ctx) sp (locatedValueBox value)
                ++ case locatedValueBox value of
                    Just (Box located) ->
                        walkValue
                            sql
                            (Ctx (Just sp) (nodeTag located : nodeTag value : ctxTags ctx))
                            located
                    Nothing -> ["Located node without a value"]

walkOpaque :: forall a. (Data a) => ByteString -> Ctx -> a -> [String]
walkOpaque sql ctx value =
    case firstChildAs value of
        Nothing -> ["OpaqueBody without a ByteSpan"]
        Just sp ->
            spanIssues sql "OpaqueBody" sp
                ++ containmentIssues "OpaqueBody" sp (ctxParent ctx)

walkChildren :: forall a. (Data a) => ByteString -> Ctx -> a -> [String]
walkChildren sql ctx value =
    concatMap
        (\(Box child) -> walkValue sql (Ctx (ctxParent ctx) (nodeTag value : ctxTags ctx)) child)
        (childrenOf value)

walkList :: forall a. (Data a) => ByteString -> Ctx -> a -> [String]
walkList sql ctx value =
    orderingIssues label hulls
        ++ concatMap (\(Box element) -> walkValue sql ctx element) boxes
    where
        boxes = listBoxes value
        hulls =
            mapMaybe
                (\(idx, Box element) -> (idx,) <$> collectHull element)
                (zip [0 :: Int ..] boxes)
        label = case ctxTags ctx of
            (tag : _) -> tagType tag <> "." <> tagConstructor tag <> " list"
            [] -> "top-level list"

childrenOf :: forall a. (Data a) => a -> [Box]
childrenOf = gmapQ Box

locatedValueBox :: forall a. (Data a) => a -> Maybe Box
locatedValueBox value = case childrenOf value of
    (_ : v : _) -> Just v
    _ -> Nothing

firstChildAs :: forall a b. (Data a, Typeable b) => a -> Maybe b
firstChildAs value = case gmapQ (cast :: forall d. (Data d) => d -> Maybe b) value of
    (Just first : _) -> Just first
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- Span checks
-- ---------------------------------------------------------------------------

spanIssues :: ByteString -> String -> ByteSpan -> [String]
spanIssues sql label sp =
    [ label <> ": empty span " <> show sp
    | spanStart sp >= spanEnd sp
    ]
        ++ [ label <> ": invalid UTF-8/bounds span " <> show sp
           | not (isNonEmptyValidSpan sql sp)
           ]

containmentIssues :: String -> ByteSpan -> Maybe ByteSpan -> [String]
containmentIssues label child = \case
    Nothing -> []
    Just parent
        | spanStart child >= spanStart parent && spanEnd child <= spanEnd parent -> []
        | otherwise ->
            [ label
                <> ": span "
                <> show child
                <> " is outside nearest parent "
                <> show parent
            ]

orderingIssues :: String -> [(Int, ByteSpan)] -> [String]
orderingIssues _ [] = []
orderingIssues label ((firstIndex, firstSpan) : rest) = go firstIndex firstSpan rest
    where
        go previousIndex previous ((currentIndex, current) : more)
            | spanStart current >= spanEnd previous = go currentIndex current more
            | otherwise =
                ( label
                    <> ": elements "
                    <> show previousIndex
                    <> " "
                    <> show previous
                    <> " and "
                    <> show currentIndex
                    <> " "
                    <> show current
                    <> " overlap or are out of source order"
                )
                    : go currentIndex current more
        go _ _ [] = []

-- ---------------------------------------------------------------------------
-- Raw-byte contracts
-- ---------------------------------------------------------------------------

rawIssues :: ByteString -> [Tag] -> ByteSpan -> Maybe Box -> [String]
rawIssues sql tags sp mBox =
    case (sliceSpan sp sql, mBox) of
        (Nothing, _) -> []
        (_, Nothing) -> []
        (Just source, Just (Box value))
            | Just namePart <- cast value ->
                exactCheck "NamePart.namePartRaw" source (namePartRaw namePart)
            | Just literal <- cast value ->
                exactCheck "Literal.literalRaw" source (literalRaw literal)
            | Just expression <- cast value -> case expression of
                ExprLiteral literal ->
                    exactCheck "ExprLiteral.literalRaw" source (literalRaw literal)
                _ -> []
            | Just raw <- cast value -> byteStringCheck source raw
            | otherwise -> []
    where
        names = concatMap (\tag -> [tagType tag, tagConstructor tag]) tags
        exactTags = ["ExprBareKeyword", "PragmaBare"]
        normalizedTags =
            [ "ExplainOption"
            , "CopyOption"
            , "TableLikeOption"
            , "AttachObject"
            , "DetachObject"
            ]
        exactCheck label source actual
            | actual == source = []
            | otherwise =
                [ label
                    <> " value "
                    <> show actual
                    <> " does not match source bytes "
                    <> show source
                    <> " at "
                    <> show sp
                ]
        byteStringCheck source raw
            | any (`elem` names) exactTags = exactCheck "raw byte string" source raw
            | any (`elem` names) normalizedTags =
                [ "normalized byte string "
                    <> show raw
                    <> " is neither the source bytes "
                    <> show source
                    <> " nor their bare ASCII upper-case form at "
                    <> show sp
                | raw /= source
                , not (isBareAsciiWord source && raw == asciiUpperBS source)
                ]
            | otherwise = []

isBareAsciiWord :: ByteString -> Bool
isBareAsciiWord bs = not (BS.null bs) && BS.all bareByte bs
    where
        bareByte :: Word8 -> Bool
        bareByte byte =
            isAsciiUpperChar byte
                || isAsciiLowerChar byte
                || isDigitChar byte
                || byte == 95 -- '_'
        isAsciiUpperChar byte = isAsciiUpper (toEnum (fromIntegral byte))
        isAsciiLowerChar byte = isAsciiLower (toEnum (fromIntegral byte))
        isDigitChar byte = isDigit (toEnum (fromIntegral byte))

-- ---------------------------------------------------------------------------
-- Data helpers
-- ---------------------------------------------------------------------------

nodeTag :: forall a. (Data a) => a -> Tag
nodeTag value =
    Tag
        { tagType = shortTypeName (dataTypeName (dataTypeOf value))
        , tagConstructor = showConstr (toConstr value)
        }

shortTypeName :: String -> String
shortTypeName name = reverse (takeWhile (/= '.') (reverse name))

isLocatedNode :: forall a. (Data a) => a -> Bool
isLocatedNode value = shortTypeName (dataTypeName (dataTypeOf value)) == "Located"

isOpaqueNode :: forall a. (Data a) => a -> Bool
isOpaqueNode value = shortTypeName (dataTypeName (dataTypeOf value)) == "OpaqueBody"

isListType :: forall a. (Data a) => a -> Bool
isListType value = shortTypeName (dataTypeName (dataTypeOf value)) == "[]"

isLeafType :: forall a. (Data a) => a -> Bool
isLeafType value =
    shortTypeName (dataTypeName (dataTypeOf value))
        `elem` [ "ByteString"
               , "Text"
               , "Char"
               , "Int"
               , "Integer"
               , "Bool"
               , "Word"
               , "Word8"
               , "Word32"
               , "Word64"
               , "Int8"
               , "Int16"
               , "Int32"
               , "Int64"
               , "Double"
               , "Float"
               , "Ordering"
               , "()"
               , "ByteSpan"
               ]

listBoxes :: forall a. (Data a) => a -> [Box]
listBoxes value
    | not (isListType value) = []
    | otherwise = go value
    where
        go :: forall x. (Data x) => x -> [Box]
        go current = case constrIndex (toConstr current) of
            1 -> []
            2 -> case gmapQ Box current of
                (Box headValue : Box tailValue : _) -> Box headValue : go tailValue
                _ -> []
            _ -> []

collectHull :: forall a. (Data a) => a -> Maybe ByteSpan
collectHull value
    | isLocatedNode value || isOpaqueNode value = firstChildAs value
    | isListType value = foldMap (\(Box element) -> collectHull element) (listBoxes value)
    | isLeafType value = Nothing
    | otherwise = foldMap (\(Box child) -> collectHull child) (childrenOf value)
