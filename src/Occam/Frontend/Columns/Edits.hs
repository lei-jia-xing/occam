-- | Single-point 'EditColumn' edit extraction and list-item span geometry.
module Occam.Frontend.Columns.Edits (
    columnEdits,
    TokenEntry (..),
    TokenTable,
    buildTokenTable,
    columnDefSpans,
    insertListItemSpans,
    valuesItemSpans,
    commaDeleteSpans,
    isInsertTainted,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Foldable qualified as Foldable
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Lazy qualified as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Word (Word8)
import Occam.Facts (EditTier (..), ParserEdit (..), ParserEditCategory (..))
import Occam.Frontend.AST.DDL qualified as AST
import Occam.Frontend.AST.DML qualified as AST
import Occam.Frontend.AST.Expr qualified as AST
import Occam.Frontend.AST.Statement qualified as AST
import Occam.Frontend.Context (
    AnalysisContext (..),
    unwrapExplainAnalyze,
 )
import Occam.Frontend.Lexer (SpannedToken, Token (..), isTriviaToken)
import Occam.Frontend.Span (ByteSpan (..), Located (..))

-- | Token and delimiter-navigation metadata for one lexer token.
data TokenEntry = TokenEntry
    { teSpan :: !ByteSpan
    -- ^ Byte span of this token, as supplied by the lexer.
    , teToken :: !Token
    -- ^ Lexed token at this entry.
    , teParenEnd :: !Int
    -- ^ End byte offset of its matching right parenthesis, or @-1@ if unmatched.
    , teParent :: !Int
    -- ^ Index of the innermost opening parenthesis containing this token, or @-1@ outside parentheses.
    }
    deriving (Eq, Show)

-- | Unit token vector annotated with matching parenthesis endpoints.
type TokenTable = Vector TokenEntry

-- | Build a 'TokenTable' from a statement's spanned token vector.
buildTokenTable :: Vector SpannedToken -> TokenTable
buildTokenTable tokens = runST do
    let !n = V.length tokens
    mv <- MV.new n
    let go !i stack
            | i >= n = pure ()
            | otherwise = do
                let L sp tok = V.unsafeIndex tokens i
                    parent = case stack of
                        openIdx : _ -> openIdx
                        [] -> -1
                MV.unsafeWrite mv i (TokenEntry sp tok (-1) parent)
                case tok of
                    TokLParen -> go (i + 1) (i : stack)
                    TokRParen -> case stack of
                        (openIdx : rest) -> do
                            openEntry <- MV.unsafeRead mv openIdx
                            MV.unsafeWrite mv openIdx openEntry {teParenEnd = spanEnd sp}
                            go (i + 1) rest
                        [] -> go (i + 1) []
                    _ -> go (i + 1) stack
    go 0 []
    V.unsafeFreeze mv

{- | Emit single-point 'EditColumn' 'ParserEdit' candidates for one statement.

Returns @[]@ when 'ctxColumnIR' is 'False'.
-}
columnEdits :: AnalysisContext -> AST.Statement -> [ParserEdit]
columnEdits ctx rawStmt
    | not (ctxColumnIR ctx) = []
    | otherwise = case unwrapExplainAnalyze rawStmt of
        AST.StatementCreate (L _ createStmt) -> case locValue (AST.createObject createStmt) of
            AST.CreateTableObject ct -> createTableColumnEdits ctx tokens ct
            _ -> []
        AST.StatementInsert (L _ ins) -> insertColumnEdits ctx tokens ins
        _ -> []
    where
        tokens = buildTokenTable (ctxUnitTokens ctx)

createTableColumnEdits :: AnalysisContext -> TokenTable -> AST.CreateTable -> [ParserEdit]
createTableColumnEdits ctx tokens ct =
    mapMaybe (\(s, e) -> mkColumnEdit ctx s e "remove-column_create_def") $
        catMaybes (columnDefSpans (ctxSource ctx) (ctxAstSpanShift ctx) tokens ct)

insertColumnEdits :: AnalysisContext -> TokenTable -> AST.InsertStmt -> [ParserEdit]
insertColumnEdits ctx tokens ins =
    explicitEdits ++ valuesEdits
    where
        !shift = ctxAstSpanShift ctx
        mExplicitSpans = insertListItemSpans shift tokens (AST.insertColumns ins)
        explicitDels = case mExplicitSpans of
            Just spans -> commaDeleteSpans tokens spans
            Nothing -> []
        explicitEdits
            | null (AST.insertColumns ins) = []
            | otherwise =
                mapMaybe
                    (\(s, e) -> mkColumnEdit ctx s e "remove-column_insert_item")
                    (catMaybes explicitDels)
        tainted =
            isInsertTainted ins
                || (not (null (AST.insertColumns ins)) && length explicitDels /= length (AST.insertColumns ins))
        valuesEdits
            | tainted = []
            | AST.InsertValues rows <- AST.insertSource ins =
                concatMap rowEdits rows
            | otherwise = []
        rowEdits row = case valuesItemSpans shift tokens row of
            Nothing -> []
            Just items ->
                mapMaybe
                    (\(s, e) -> mkColumnEdit ctx s e "remove-column_values_item")
                    (catMaybes (commaDeleteSpans tokens items))

-- | Whether an 'AST.InsertStmt' has exotic clauses that forbid positional @VALUES@ binding.
isInsertTainted :: AST.InsertStmt -> Bool
isInsertTainted ins =
    AST.insertReplace ins
        || not (null (AST.insertPartitions ins))
        || maybe False (isTaintedOrAction . locValue) (AST.insertOrAction ins)
        || isJust (AST.insertOnConflict ins)
        || isJust (AST.insertOnDuplicate ins)
        || not (null (AST.insertReturning ins))
    where
        isTaintedOrAction = \case
            AST.InsertOrIgnore -> True
            AST.InsertOrReplace -> True
            _ -> False

mkColumnEdit :: AnalysisContext -> Int -> Int -> Text -> Maybe ParserEdit
mkColumnEdit ctx start end label
    | start < unitStart || end > unitEnd || start >= end = Nothing
    | otherwise = Just (ParserEdit start end "" EditColumn TierDefault label False False False False)
    where
        ByteSpan unitStart unitEnd = ctxUnitSpan ctx

-- | Index each table body once, then resolve column names to their deletion spans.
columnDefSpans :: ByteString -> Int -> TokenTable -> AST.CreateTable -> [Maybe (Int, Int)]
columnDefSpans source shift tokens ct =
    [ do
        bounds <- body
        spans <- Map.lookup bounds bodies
        IntMap.lookup start spans
    | (start, body) <- columns
    ]
    where
        columns =
            [ (start, enclosingParentheses tokens start (shift + spanEnd nameSpan))
            | L _ (AST.TableElementColumn (L _ col)) <- AST.createTableElements ct
            , let nameSpan = locSpan (AST.columnName col)
                  start = shift + spanStart nameSpan
            ]
        bodies =
            Map.fromList
                [ (bounds, IntMap.fromDistinctAscList (tableBodySpans source tokens (open + 1) (close - 1)))
                | (_, Just bounds@(open, close)) <- columns
                ]

-- | Source spans of an @INSERT@ explicit column list, in order.
insertListItemSpans :: Int -> TokenTable -> [Located AST.NamePart] -> Maybe [(Int, Int)]
insertListItemSpans shift tokens cols = do
    let spans = [(shift + spanStart sp, shift + spanEnd sp) | L sp _ <- cols]
    case spans of
        (firstSpan : _ : _) -> do
            let lastEnd = Foldable.foldl' (\_ (_, e) -> e) (snd firstSpan) spans
            (open, close) <- enclosingParentheses tokens (fst firstSpan) lastEnd
            guard (all (\(s, e) -> s > open && e < close) spans)
            pure spans
        _ -> pure spans

-- | Source spans of one @VALUES@ row's items, verifying every item sits inside the row's parentheses.
valuesItemSpans :: Int -> TokenTable -> [Located AST.Expr] -> Maybe [(Int, Int)]
valuesItemSpans _ _ [] = Just []
valuesItemSpans shift tokens items@(firstItem : _) = do
    let spans = [(shift + spanStart sp, shift + spanEnd sp) | L sp _ <- items]
        !firstStart = shift + spanStart (locSpan firstItem)
        !lastEnd = Foldable.foldl' (\_ (s, e) -> max s e) firstStart spans
    (rowOpen, rowClose) <- innermostParentheses tokens firstStart lastEnd
    guard (all (\(s, e) -> s >= rowOpen && e <= rowClose) spans)
    pure spans

-- | Comma-gated deletion spans for items in a comma-separated list.
commaDeleteSpans :: TokenTable -> [(Int, Int)] -> [Maybe (Int, Int)]
commaDeleteSpans _ [] = []
commaDeleteSpans _ [_] = [Nothing]
commaDeleteSpans tokens spans =
    zipWith itemRange [0 :: Int ..] spans
    where
        vec = V.fromList spans
        itemRange !i (s, e) = do
            let range =
                    if i == 0
                        then (s, fst (V.unsafeIndex vec 1))
                        else (snd (V.unsafeIndex vec (i - 1)), e)
            guard (uncurry (<) range)
            _ <- uncurry (findComma tokens) range
            pure range

-- ---------------------------------------------------------------------------
-- Token navigation helpers
-- ---------------------------------------------------------------------------

partitionPoint :: (TokenEntry -> Bool) -> TokenTable -> Int
partitionPoint predicate vec = go 0 (V.length vec)
    where
        go !lo !hi
            | lo >= hi = lo
            | otherwise =
                let !mid = lo + ((hi - lo) `div` 2)
                 in if predicate (V.unsafeIndex vec mid)
                        then go (mid + 1) hi
                        else go lo mid

enclosingParentheses :: TokenTable -> Int -> Int -> Maybe (Int, Int)
enclosingParentheses tokens start end =
    Foldable.foldl' (\_ bounds -> Just bounds) Nothing (containingParentheses tokens start end)

innermostParentheses :: TokenTable -> Int -> Int -> Maybe (Int, Int)
innermostParentheses tokens start end =
    listToMaybe (containingParentheses tokens start end)

containingParentheses :: TokenTable -> Int -> Int -> [(Int, Int)]
containingParentheses tokens start end =
    case tokens V.!? idx0 of
        Just entry -> go (teParent entry)
        Nothing -> []
    where
        !idx0 = partitionPoint (\entry -> spanEnd (teSpan entry) <= start) tokens
        go !idx
            | idx < 0 = []
            | otherwise =
                let entry = V.unsafeIndex tokens idx
                    !open = spanStart (teSpan entry)
                    !close = teParenEnd entry
                    rest = go (teParent entry)
                 in if open < start && close >= end
                        then (open, close) : rest
                        else rest

tableBodySpans :: ByteString -> TokenTable -> Int -> Int -> [(Int, (Int, Int))]
tableBodySpans source tokens bodyStart bodyEnd = go idx0 bodyStart Nothing
    where
        !n = V.length tokens
        !idx0 = partitionPoint (\e -> spanEnd (teSpan e) <= bodyStart) tokens
        go !idx !itemStart !first
            | idx >= n = emit itemStart bodyEnd first []
            | otherwise =
                let entry = V.unsafeIndex tokens idx
                    ByteSpan s e = teSpan entry
                    tok = teToken entry
                 in if s >= bodyEnd
                        then emit itemStart bodyEnd first []
                        else case tok of
                            TokComma ->
                                emit itemStart s first (go (idx + 1) e Nothing)
                            TokLParen ->
                                let !pEnd = teParenEnd entry
                                 in if pEnd < 0 || pEnd > bodyEnd
                                        then []
                                        else go (partitionPoint (\c -> spanStart (teSpan c) < pEnd) tokens) itemStart (first <|> Just s)
                            _
                                | isTriviaToken tok -> go (idx + 1) itemStart first
                                | otherwise -> go (idx + 1) itemStart (first <|> Just s)
        emit itemStart itemEnd first rest = case first of
            Just start
                | itemStart > bodyStart ->
                    (start, (precedingWhitespace source (itemStart - 1), itemEnd)) : rest
                | itemEnd < bodyEnd -> (start, (itemStart, itemEnd + 1)) : rest
            _ -> rest

findComma :: TokenTable -> Int -> Int -> Maybe (Int, Int)
findComma tokens start end = go idx0
    where
        !n = V.length tokens
        !idx0 = partitionPoint (\e -> spanStart (teSpan e) < start) tokens
        go !idx
            | idx >= n = Nothing
            | otherwise =
                let entry = V.unsafeIndex tokens idx
                    ByteSpan s e = teSpan entry
                 in if s >= end
                        then Nothing
                        else case teToken entry of
                            TokLParen ->
                                let !pEnd = teParenEnd entry
                                 in if pEnd < 0 || pEnd > end
                                        then Nothing
                                        else go (partitionPoint (\c -> spanEnd (teSpan c) <= pEnd) tokens)
                            TokComma | e <= end -> Just (s, e)
                            _ -> go (idx + 1)

precedingWhitespace :: ByteString -> Int -> Int
precedingWhitespace source = go
    where
        !len = BS.length source
        go !pos
            | pos <= 0 || pos > len = pos
            | isAsciiSpace (BSU.unsafeIndex source (pos - 1)) = go (pos - 1)
            | otherwise = pos
        isAsciiSpace :: Word8 -> Bool
        isAsciiSpace w = w == 32 || w == 9 || w == 13 || w == 10
