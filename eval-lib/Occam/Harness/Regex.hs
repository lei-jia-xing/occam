-- | Pure Haskell Thompson NFA regex matcher for crash signature patterns.
module Occam.Harness.Regex (
    CompiledRegex,
    compileRegex,
    matchExpectedSignature,
    searchRegex,
) where

import Data.Char (isAlphaNum, isDigit, isSpace, toLower)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Text (Text)
import Data.Text qualified as T

data Anchor
    = AnchorStart
    | AnchorEnd
    | AnchorWordBoundary
    | AnchorNonWordBoundary
    deriving (Eq, Show)

data CharSetItem
    = CsChar !Char
    | CsRange !Char !Char
    | CsDigit
    | CsNonDigit
    | CsSpace
    | CsNonSpace
    | CsWord
    | CsNonWord
    deriving (Eq, Show)

data CharMatcher
    = MatchAny
    | MatchLiteral !Char
    | MatchClass !Bool ![CharSetItem]
    deriving (Eq, Show)

data AST
    = ASTEmpty
    | ASTMatcher !CharMatcher
    | ASTAnchor !Anchor
    | ASTSeq ![AST]
    | ASTAlt !AST !AST
    | ASTStar !AST
    | ASTOpt !AST
    deriving (Eq, Show)

data NfaNode
    = Step !CharMatcher !Int
    | Split !Int !Int
    | Assert !Anchor !Int
    | Match
    deriving (Eq, Show)

-- | Compiled linear-time NFA regular expression (case-insensitive, DOTALL).
data CompiledRegex = CompiledRegex
    { crStart :: !Int
    , crNodes :: !(IntMap NfaNode)
    }
    deriving (Eq, Show)

-- | Compile a signature regex pattern (supporting ERE, non-capturing groups, and common Python escapes).
compileRegex :: Text -> Either String CompiledRegex
compileRegex pat = do
    (ast, rest) <- parseAlt (T.unpack pat)
    if null rest
        then pure (buildNfa ast)
        else Left ("Unexpected trailing pattern token: " <> take 16 rest)

-- | Search anywhere within text (case-insensitive, @.@ matches newlines).
searchRegex :: CompiledRegex -> Text -> Bool
searchRegex CompiledRegex {crStart, crNodes} input =
    loop input initActive
    where
        firstNext = fst <$> T.uncons input
        initActive = epsClosure crNodes Nothing firstNext (IS.singleton crStart)

        loop remaining active
            | isMatched crNodes active = True
            | otherwise = case T.uncons remaining of
                Nothing -> False
                Just (c, nextTxt) ->
                    let nxtChar = fst <$> T.uncons nextTxt
                        stepped = stepChar crNodes c active
                        reseeded = IS.insert crStart stepped
                        closed = epsClosure crNodes (Just c) nxtChar reseeded
                     in loop nextTxt closed

-- | Evaluate @--expected-signature@ against candidate scope text, falling back to literal match on invalid regexes.
matchExpectedSignature :: Text -> Text -> (Bool, Bool)
matchExpectedSignature sigText scopeText =
    case compileRegex sigText of
        Right compiled -> (searchRegex compiled scopeText, True)
        Left _ -> (T.toLower sigText `T.isInfixOf` T.toLower scopeText, False)

isMatched :: IntMap NfaNode -> IntSet -> Bool
isMatched nodes = any (\sid -> IM.lookup sid nodes == Just Match) . IS.toList

stepChar :: IntMap NfaNode -> Char -> IntSet -> IntSet
stepChar nodes c = IS.foldl' collect IS.empty
    where
        collect acc sid = case IM.lookup sid nodes of
            Just (Step m nxt) | matchesChar m c -> IS.insert nxt acc
            _ -> acc

epsClosure :: IntMap NfaNode -> Maybe Char -> Maybe Char -> IntSet -> IntSet
epsClosure nodes prevChar nextChar seeds = dfs IS.empty (IS.toList seeds)
    where
        dfs visited [] = visited
        dfs visited (sid : queue)
            | IS.member sid visited = dfs visited queue
            | otherwise =
                let visited' = IS.insert sid visited
                 in case IM.lookup sid nodes of
                        Just (Split a b) -> dfs visited' (a : b : queue)
                        Just (Assert anch nxt)
                            | checkAnchor anch prevChar nextChar -> dfs visited' (nxt : queue)
                            | otherwise -> dfs visited' queue
                        _ -> dfs visited' queue

checkAnchor :: Anchor -> Maybe Char -> Maybe Char -> Bool
checkAnchor AnchorStart Nothing _ = True
checkAnchor AnchorStart (Just '\n') _ = True
checkAnchor AnchorStart _ _ = False
checkAnchor AnchorEnd _ Nothing = True
checkAnchor AnchorEnd _ (Just '\n') = True
checkAnchor AnchorEnd _ _ = False
checkAnchor AnchorWordBoundary prevChar nextChar =
    maybe False isRegexWord prevChar /= maybe False isRegexWord nextChar
checkAnchor AnchorNonWordBoundary prevChar nextChar =
    maybe False isRegexWord prevChar == maybe False isRegexWord nextChar

matchesChar :: CharMatcher -> Char -> Bool
matchesChar MatchAny _ = True
matchesChar (MatchLiteral target) c = toLower target == toLower c
matchesChar (MatchClass negated items) c =
    let hit = any (`matchSetItem` c) items
     in if negated then not hit else hit

matchSetItem :: CharSetItem -> Char -> Bool
matchSetItem (CsChar target) c = toLower target == toLower c
matchSetItem (CsRange lo hi) c =
    let lc = toLower c
     in (lc >= toLower lo && lc <= toLower hi) || (c >= lo && c <= hi)
matchSetItem CsDigit c = isDigit c
matchSetItem CsNonDigit c = not (isDigit c)
matchSetItem CsSpace c = isSpace c
matchSetItem CsNonSpace c = not (isSpace c)
matchSetItem CsWord c = isRegexWord c
matchSetItem CsNonWord c = not (isRegexWord c)

isRegexWord :: Char -> Bool
isRegexWord c = isAlphaNum c || c == '_'

-- | Recursive-descent regex parser.
parseAlt :: String -> Either String (AST, String)
parseAlt src = do
    (lhs, rest) <- parseSeq src
    case rest of
        ('|' : afterBar) -> do
            (rhs, afterRhs) <- parseAlt afterBar
            pure (ASTAlt lhs rhs, afterRhs)
        _ -> pure (lhs, rest)

parseSeq :: String -> Either String (AST, String)
parseSeq = go []
    where
        go acc [] = pure (seqAst (reverse acc), [])
        go acc rest@(c : _)
            | c == '|' || c == ')' = pure (seqAst (reverse acc), rest)
            | otherwise = do
                (term, afterTerm) <- parseQuantified rest
                go (term : acc) afterTerm

        seqAst [] = ASTEmpty
        seqAst [single] = single
        seqAst xs = ASTSeq xs

parseQuantified :: String -> Either String (AST, String)
parseQuantified src = do
    (atom, rest) <- parseAtom src
    applyQuantifiers atom rest

applyQuantifiers :: AST -> String -> Either String (AST, String)
applyQuantifiers atom rest = case rest of
    ('*' : tl) -> continue (ASTStar atom) (stripLazy tl)
    ('+' : tl) -> continue (ASTSeq [atom, ASTStar atom]) (stripLazy tl)
    ('?' : tl) -> continue (ASTOpt atom) (stripLazy tl)
    ('{' : tl) -> case parseBraceCount tl of
        Nothing -> pure (atom, rest)
        Just (minRep, maxRep, afterBrace) -> do
            expanded <- expandBounds atom minRep maxRep
            continue expanded (stripLazy afterBrace)
    _ -> pure (atom, rest)
    where
        stripLazy ('?' : tl) = tl
        stripLazy xs = xs

        continue nextAtom remaining
            | isQuantStart remaining = Left "Nothing to repeat or nested quantifier without group"
            | otherwise = pure (nextAtom, remaining)

        isQuantStart ('*' : _) = True
        isQuantStart ('+' : _) = True
        isQuantStart ('?' : _) = True
        isQuantStart _ = False

expandBounds :: AST -> Int -> Maybe Int -> Either String AST
expandBounds atom minRep Nothing
    | minRep > 200 = Left "Regex quantifier lower bound exceeds 200"
    | otherwise = pure (ASTSeq (replicate minRep atom <> [ASTStar atom]))
expandBounds atom minRep (Just maxRep)
    | maxRep < minRep = Left "Invalid quantifier range {m,n} with n < m"
    | maxRep > 200 = Left "Regex quantifier upper bound exceeds 200"
    | otherwise =
        pure (ASTSeq (replicate minRep atom <> replicate (maxRep - minRep) (ASTOpt atom)))

parseBraceCount :: String -> Maybe (Int, Maybe Int, String)
parseBraceCount src =
    let (mDigits, rest1) = span isDigit src
     in case (mDigits, rest1) of
            (_ : _, '}' : after) -> Just (read mDigits, Just (read mDigits), after)
            (_ : _, ',' : '}' : after) -> Just (read mDigits, Nothing, after)
            (_ : _, ',' : rest2) ->
                let (nDigits, rest3) = span isDigit rest2
                 in case (nDigits, rest3) of
                        (_ : _, '}' : after) -> Just (read mDigits, Just (read nDigits), after)
                        _ -> Nothing
            _ -> Nothing

parseAtom :: String -> Either String (AST, String)
parseAtom [] = Left "Unexpected end of regex"
parseAtom (c : rest) = case c of
    '*' -> Left "Nothing to repeat before '*'"
    '+' -> Left "Nothing to repeat before '+'"
    '?' -> Left "Nothing to repeat before '?'"
    '.' -> pure (ASTMatcher MatchAny, rest)
    '^' -> pure (ASTAnchor AnchorStart, rest)
    '$' -> pure (ASTAnchor AnchorEnd, rest)
    '[' -> parseCharClass rest
    '(' -> parseGroup rest
    '\\' -> parseEscape rest
    _ -> pure (ASTMatcher (MatchLiteral c), rest)

parseGroup :: String -> Either String (AST, String)
parseGroup src = case src of
    ('?' : ':' : inner) -> closeGroup inner
    ('?' : _) -> Left "Unsupported (?...) extension in regex"
    _ -> closeGroup src
    where
        closeGroup body = do
            (innerAst, afterInner) <- parseAlt body
            case afterInner of
                (')' : rest) -> pure (innerAst, rest)
                _ -> Left "Unbalanced '(' in regex"

parseEscape :: String -> Either String (AST, String)
parseEscape [] = Left "Trailing backslash in regex"
parseEscape (c : rest) = case c of
    'd' -> pure (ASTMatcher (MatchClass False [CsDigit]), rest)
    'D' -> pure (ASTMatcher (MatchClass False [CsNonDigit]), rest)
    's' -> pure (ASTMatcher (MatchClass False [CsSpace]), rest)
    'S' -> pure (ASTMatcher (MatchClass False [CsNonSpace]), rest)
    'w' -> pure (ASTMatcher (MatchClass False [CsWord]), rest)
    'W' -> pure (ASTMatcher (MatchClass False [CsNonWord]), rest)
    'b' -> pure (ASTAnchor AnchorWordBoundary, rest)
    'B' -> pure (ASTAnchor AnchorNonWordBoundary, rest)
    'n' -> pure (ASTMatcher (MatchLiteral '\n'), rest)
    'r' -> pure (ASTMatcher (MatchLiteral '\r'), rest)
    't' -> pure (ASTMatcher (MatchLiteral '\t'), rest)
    _ -> pure (ASTMatcher (MatchLiteral c), rest)

parseCharClass :: String -> Either String (AST, String)
parseCharClass src = do
    let (negated, body0) = case src of
            ('^' : rest) -> (True, rest)
            _ -> (False, src)
        (firstChar, body1) = case body0 of
            (']' : rest) -> ([CsChar ']'], rest)
            ('-' : rest) -> ([CsChar '-'], rest)
            _ -> ([], body0)
    (items, remaining) <- go firstChar body1
    pure (ASTMatcher (MatchClass negated items), remaining)
    where
        go _ [] = Left "Unterminated '[' character class"
        go acc (']' : rest) = pure (reverse acc, rest)
        go acc ('\\' : c : rest) = do
            item <- escapeToClassItem c
            checkRange acc item rest
        go _ ['\\'] = Left "Trailing backslash in character class"
        go acc (c : rest) = checkRange acc (CsChar c) rest

        checkRange acc item@(CsChar lo) ('-' : nextC : rest)
            | nextC /= ']' = case nextC of
                '\\' -> case rest of
                    (escC : rest') ->
                        let hi = unescapeClassLit escC
                         in if lo > hi
                                then Left "Out-of-order character class range"
                                else go (CsRange lo hi : acc) rest'
                    [] -> Left "Unterminated character class"
                _ ->
                    if lo > nextC
                        then Left "Out-of-order character class range"
                        else go (CsRange lo nextC : acc) rest
            | otherwise = go (item : acc) ('-' : nextC : rest)
        checkRange acc item rest = go (item : acc) rest

        unescapeClassLit 'n' = '\n'
        unescapeClassLit 'r' = '\r'
        unescapeClassLit 't' = '\t'
        unescapeClassLit other = other

        escapeToClassItem 'd' = Right CsDigit
        escapeToClassItem 'D' = Right CsNonDigit
        escapeToClassItem 's' = Right CsSpace
        escapeToClassItem 'S' = Right CsNonSpace
        escapeToClassItem 'w' = Right CsWord
        escapeToClassItem 'W' = Right CsNonWord
        escapeToClassItem 'n' = Right (CsChar '\n')
        escapeToClassItem 'r' = Right (CsChar '\r')
        escapeToClassItem 't' = Right (CsChar '\t')
        escapeToClassItem other = Right (CsChar other)

buildNfa :: AST -> CompiledRegex
buildNfa root =
    let (startId, nodes, _) = compileAst root matchId IM.empty (matchId + 1)
     in CompiledRegex {crStart = startId, crNodes = IM.insert matchId Match nodes}
    where
        matchId = 0

        compileAst ASTEmpty target nodes nextId = (target, nodes, nextId)
        compileAst (ASTMatcher m) target nodes nextId =
            (nextId, IM.insert nextId (Step m target) nodes, nextId + 1)
        compileAst (ASTAnchor a) target nodes nextId =
            (nextId, IM.insert nextId (Assert a target) nodes, nextId + 1)
        compileAst (ASTSeq xs) target nodes nextId =
            foldMstep xs target nodes nextId
        compileAst (ASTAlt a b) target nodes nextId =
            let splitId = nextId
                (aEntry, nodes1, next1) = compileAst a target nodes (nextId + 1)
                (bEntry, nodes2, next2) = compileAst b target nodes1 next1
             in (splitId, IM.insert splitId (Split aEntry bEntry) nodes2, next2)
        compileAst (ASTOpt inner) target nodes nextId =
            let splitId = nextId
                (inEntry, nodes1, next1) = compileAst inner target nodes (nextId + 1)
             in (splitId, IM.insert splitId (Split inEntry target) nodes1, next1)
        compileAst (ASTStar inner) target nodes nextId =
            let splitId = nextId
                (inEntry, nodes1, next1) = compileAst inner splitId nodes (nextId + 1)
             in (splitId, IM.insert splitId (Split inEntry target) nodes1, next1)

        foldMstep xs target nodes nextId =
            foldr
                (\ast (t, nMap, nid) -> compileAst ast t nMap nid)
                (target, nodes, nextId)
                xs
