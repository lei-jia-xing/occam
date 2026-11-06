{-# LANGUAGE ForeignFunctionInterface #-}

module Occam.TreeSitter (
    -- * Parser & Tree Lifecycle
    Parser,
    Tree,
    withParser,
    parseSQL,
    freeTree,
    withParsedSQL,

    -- * Node Query
    TSNode,
    rootNode,
    nodeType,
    nodeSpan,
    nodeChildren,
    nodeNamedChildren,
    nodeText,
    isNullNode,
    isNamedNode,
    collectDescendants,
    cutSpan,
    cutNode,
    replaceSpan,
    replaceNode,

    -- * Multi-Statement Slicing
    StatementUnit (..),
    extractStatementUnits,
    extractStatements,
    countStatements,
) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.List (find, sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Foreign (Ptr, Word32, allocaBytes, castPtr, nullPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CUInt (..))
import Occam.Core (Span (..), Statement (..), StatementId (..))
import Occam.SQLLexer (SQLToken (..), lexSQL, sqlSemicolons)

-- ----------------------------------------------------------------------------
-- FFI Types & Declarations
-- ----------------------------------------------------------------------------

data CTSParser
data CTSTree

newtype Parser = Parser (Ptr CTSParser)
newtype Tree = Tree (Ptr CTSTree)

-- TSNode is 32 bytes in C99 (uint32_t[4] + void* + void*)
newtype TSNode = TSNode ByteString
    deriving (Eq, Show)

data StatementUnit = StatementUnit
    { unitStatement :: !Statement
    , unitNode :: !(Maybe TSNode)
    }
    deriving (Eq, Show)

foreign import ccall "occam_parser_new"
    c_parser_new :: IO (Ptr CTSParser)

foreign import ccall "occam_parser_free"
    c_parser_free :: Ptr CTSParser -> IO ()

foreign import ccall "occam_parse_sql"
    c_parse_sql :: Ptr CTSParser -> CString -> CUInt -> IO (Ptr CTSTree)

foreign import ccall "occam_tree_free"
    c_tree_free :: Ptr CTSTree -> IO ()

foreign import ccall "occam_tree_root_p"
    c_tree_root_p :: Ptr CTSTree -> Ptr () -> IO ()

foreign import ccall "occam_node_type_p"
    c_node_type_p :: Ptr () -> IO CString

foreign import ccall "occam_node_start_byte_p"
    c_node_start_byte_p :: Ptr () -> IO Word32

foreign import ccall "occam_node_end_byte_p"
    c_node_end_byte_p :: Ptr () -> IO Word32

foreign import ccall "occam_node_child_count_p"
    c_node_child_count_p :: Ptr () -> IO Word32

foreign import ccall "occam_node_child_p"
    c_node_child_p :: Ptr () -> Word32 -> Ptr () -> IO ()

foreign import ccall "occam_node_is_named_p"
    c_node_is_named_p :: Ptr () -> IO CBool

foreign import ccall "occam_node_is_null_p"
    c_node_is_null_p :: Ptr () -> IO CBool

-- ----------------------------------------------------------------------------
-- Safe Node Memory Helpers
-- ----------------------------------------------------------------------------

nodeSize :: Int
nodeSize = 32

withNodePtr :: TSNode -> (Ptr () -> IO a) -> IO a
withNodePtr (TSNode bs) action = BSU.unsafeUseAsCString bs (action . castPtr)

allocNode :: (Ptr () -> IO ()) -> IO TSNode
allocNode action = allocaBytes nodeSize $ \ptr -> do
    action ptr
    bs <- BS.packCStringLen (Foreign.castPtr ptr, nodeSize)
    pure (TSNode bs)

-- ----------------------------------------------------------------------------
-- High-Level API
-- ----------------------------------------------------------------------------

withParser :: (Parser -> IO a) -> IO a
withParser = bracket create destroy
    where
        create = do
            p <- c_parser_new
            if p == nullPtr
                then fail "Failed to initialize Tree-sitter SQL parser"
                else pure (Parser p)
        destroy (Parser p) = c_parser_free p

parseSQL :: Parser -> ByteString -> IO (Maybe Tree)
parseSQL (Parser p) sql =
    BSU.unsafeUseAsCStringLen sql $ \(cstr, len) -> do
        treePtr <- c_parse_sql p cstr (fromIntegral len)
        if treePtr == nullPtr
            then pure Nothing
            else pure (Just (Tree treePtr))

freeTree :: Tree -> IO ()
freeTree (Tree t) = c_tree_free t

withParsedSQL :: Parser -> ByteString -> (Tree -> IO a) -> IO (Maybe a)
withParsedSQL parser sql action = do
    mTree <- parseSQL parser sql
    case mTree of
        Nothing -> pure Nothing
        Just tree -> bracket (pure tree) freeTree (fmap Just . action)

rootNode :: Tree -> IO TSNode
rootNode (Tree t) = allocNode (c_tree_root_p t)

nodeType :: TSNode -> IO Text
nodeType node = withNodePtr node $ \p -> do
    cstr <- c_node_type_p p
    TE.decodeUtf8 <$> BS.packCString cstr

nodeSpan :: TSNode -> IO Span
nodeSpan node = withNodePtr node $ \p -> do
    s <- c_node_start_byte_p p
    e <- c_node_end_byte_p p
    pure $ Span (fromIntegral s) (fromIntegral e)

isNullNode :: TSNode -> IO Bool
isNullNode node = withNodePtr node $ \p -> do
    CBool b <- c_node_is_null_p p
    pure (b /= 0)

nodeChildren :: TSNode -> IO [TSNode]
nodeChildren node = withNodePtr node $ \p -> do
    count <- c_node_child_count_p p
    if count == 0
        then pure []
        else mapM (fetchChild p) [0 .. count - 1]
    where
        fetchChild parentPtr idx = allocNode $ \outPtr ->
            c_node_child_p parentPtr idx outPtr

isNamedNode :: TSNode -> IO Bool
isNamedNode node = withNodePtr node $ \p -> do
    CBool b <- c_node_is_named_p p
    pure (b /= 0)

nodeNamedChildren :: TSNode -> IO [TSNode]
nodeNamedChildren node = do
    allCh <- nodeChildren node
    filterChildren isNamedNode allCh
    where
        filterChildren _ [] = pure []
        filterChildren predicate (x : xs) = do
            matches <- predicate x
            rest <- filterChildren predicate xs
            pure (if matches then x : rest else rest)

nodeText :: ByteString -> TSNode -> IO ByteString
nodeText fullSrc node = do
    Span start end <- nodeSpan node
    pure $ BS.take (end - start) (BS.drop start fullSrc)

-- ----------------------------------------------------------------------------
-- Byte-Span Text Surgery (Zero Unparser)
-- ----------------------------------------------------------------------------

cutSpan :: Int -> Int -> ByteString -> ByteString
cutSpan start end src = BS.take start src <> BS.drop end src

cutNode :: TSNode -> ByteString -> IO ByteString
cutNode node src = do
    Span start end <- nodeSpan node
    pure $ cutSpan start end src

replaceSpan :: Int -> Int -> ByteString -> ByteString -> ByteString
replaceSpan start end replacement src =
    BS.take start src <> replacement <> BS.drop end src

replaceNode :: TSNode -> ByteString -> ByteString -> IO ByteString
replaceNode node replacement src = do
    Span start end <- nodeSpan node
    pure $ replaceSpan start end replacement src

-- ----------------------------------------------------------------------------
-- Multi-Statement Slicing
-- ----------------------------------------------------------------------------

-- | Recursively collect all descendant nodes satisfying a predicate.
collectDescendants :: (Text -> Bool) -> TSNode -> IO [TSNode]
collectDescendants predNode node = do
    t <- nodeType node
    children <- nodeNamedChildren node
    subMatches <- concat <$> mapM (collectDescendants predNode) children
    pure $ if predNode t then node : subMatches else subMatches

extractStatementUnits :: Tree -> ByteString -> IO [StatementUnit]
extractStatementUnits tree src = do
    root <- rootNode tree
    children <- nodeChildren root
    rawSeparators <- concat <$> mapM separatorsForRoot children
    candidates <- concat <$> mapM candidatesForRoot children
    comments <- concat <$> mapM commentsForRoot children
    let lexicalSemicolons = Set.fromList [start | Span start _ <- sqlSemicolons src]
        separators = filter (\(Span start _) -> Set.member start lexicalSemicolons) rawSeparators
        segments = statementSegments (BS.length src) separators
        candidateGroups = spansBySegment segments (sortOn (spanStartByte . fst) candidates)
        commentGroups =
            map (map fst) $
                spansBySegment
                    segments
                    (map (\commentSpan -> (commentSpan, commentSpan)) (sortOn spanStartByte comments))
    units <- catMaybes <$> sequence (zipWith3 makeUnit candidateGroups commentGroups segments)
    pure $
        zipWith
            (\idx unit -> unit {unitStatement = (unitStatement unit) {stmtId = StatementId idx}})
            [1 ..]
            units
    where
        separatorsForRoot node = do
            t <- nodeType node
            case t of
                ";" -> (: []) <$> nodeSpan node
                "transaction" -> separatorsInTransaction node
                "ERROR" -> separatorsForError node
                "comment" -> pure []
                _ -> pure []

        separatorsInTransaction node = do
            children <- nodeChildren node
            concat <$> mapM separatorsInTransactionChild children

        separatorsInTransactionChild node = do
            t <- nodeType node
            case t of
                ";" -> (: []) <$> nodeSpan node
                "ERROR" -> separatorsForError node
                "transaction" -> separatorsInTransaction node
                _ -> pure []

        separatorsForError node = do
            trigger <- containsTriggerKeyword node
            if trigger
                then do
                    separators <- separatorsInError node
                    terminator <- triggerTerminator node
                    pure $
                        case terminator of
                            Just (Span triggerEnd _) -> filter ((>= triggerEnd) . spanStartByte) separators
                            Nothing -> takeLast separators
                else do
                    control <- isTransactionControlError node
                    separators <- separatorsInError node
                    pure (if control then separators else takeLast separators)

        triggerTerminator node = do
            raw <- nodeText src node
            Span errorStart _ <- nodeSpan node
            pure $ do
                Span start end <- triggerTerminatorInTokens (lexSQL raw)
                pure (Span (errorStart + start) (errorStart + end))

        triggerTerminatorInTokens tokens = do
            afterTrigger <- afterKeyword "trigger" tokens
            afterBody <- afterKeyword "begin" afterTrigger
            findOuterEnd 0 afterBody

        afterKeyword _ [] = Nothing
        afterKeyword keyword (WordToken word : rest)
            | keyword == word = Just rest
        afterKeyword keyword (_ : rest) = afterKeyword keyword rest

        -- ponytail: token-balances CASE/END; use a dialect grammar if trigger syntax needs broader coverage.
        findOuterEnd _ [] = Nothing
        findOuterEnd caseDepth (WordToken "case" : rest) =
            findOuterEnd (caseDepth + 1) rest
        findOuterEnd caseDepth (WordToken "end" : rest)
            | caseDepth > 0 = findOuterEnd (caseDepth - 1) rest
            | otherwise = findSemicolon rest
        findOuterEnd caseDepth (_ : rest) = findOuterEnd caseDepth rest

        findSemicolon [] = Nothing
        findSemicolon (SemicolonToken semicolonSpan : _) = Just semicolonSpan
        findSemicolon (_ : rest) = findSemicolon rest

        isTransactionControlError node = do
            raw <- nodeText src node
            pure $
                case lexSQL raw of
                    WordToken "start" : WordToken "transaction" : _ -> True
                    WordToken command : _ ->
                        command `elem` ["begin", "commit", "end", "rollback", "abort", "savepoint", "release"]
                    _ -> False

        separatorsInError node = do
            children <- nodeChildren node
            concat <$> mapM separatorsInErrorChild children

        separatorsInErrorChild node = do
            t <- nodeType node
            case t of
                ";" -> (: []) <$> nodeSpan node
                "ERROR" -> separatorsInError node
                _ -> pure []

        containsTriggerKeyword node = do
            t <- nodeType node
            if t == "keyword_trigger"
                then pure True
                else
                    if t /= "ERROR"
                        then pure False
                        else do
                            children <- nodeChildren node
                            anyM
                                ( \child -> do
                                    childType <- nodeType child
                                    if childType == "keyword_trigger" || childType == "ERROR"
                                        then containsTriggerKeyword child
                                        else pure False
                                )
                                children

        anyM _ [] = pure False
        anyM predicate (item : rest) = do
            matches <- predicate item
            if matches then pure True else anyM predicate rest

        takeLast [] = []
        takeLast values = [last values]

        candidatesForRoot node = do
            t <- nodeType node
            case t of
                ";" -> pure []
                "comment" -> pure []
                "transaction" -> candidatesInTransaction node
                "ERROR" -> candidatesInError node
                _ -> (: []) <$> candidate node

        candidatesInTransaction node = do
            children <- nodeChildren node
            concat <$> mapM candidatesInTransactionChild children

        candidatesInTransactionChild node = do
            t <- nodeType node
            case t of
                ";" -> pure []
                "comment" -> pure []
                "ERROR" -> candidatesInError node
                "transaction" -> candidatesInTransaction node
                _ -> (: []) <$> candidate node

        candidatesInError node = do
            current <- candidate node
            children <- nodeChildren node
            nested <- concat <$> mapM errorCandidateChild children
            pure (current : nested)

        errorCandidateChild node = do
            t <- nodeType node
            if t == "ERROR"
                then candidatesInError node
                else
                    if t == "statement" || "keyword_" `T.isPrefixOf` t
                        then (: []) <$> candidate node
                        else pure []

        candidate node = do
            statementSpan <- nodeSpan node
            pure (statementSpan, node)

        commentsForRoot node = do
            t <- nodeType node
            case t of
                "comment" -> (: []) <$> nodeSpan node
                "transaction" -> commentsInContainer node
                "ERROR" -> commentsInErrors node
                _ -> pure []

        commentsInContainer node = do
            children <- nodeChildren node
            concat <$> mapM commentChild children

        commentsInErrors node = do
            children <- nodeChildren node
            concat <$> mapM commentChild children

        commentChild node = do
            t <- nodeType node
            case t of
                "comment" -> (: []) <$> nodeSpan node
                "ERROR" -> commentsInErrors node
                "transaction" -> commentsInContainer node
                _ -> pure []

        makeUnit overlapping comments (Span boundStart boundEnd) = do
            let starts =
                    [ candidateStart
                    | (Span candidateStart _, _) <- overlapping
                    , candidateStart >= boundStart
                    , candidateStart < boundEnd
                    ]
                ends =
                    [ min candidateEnd boundEnd
                    | (Span candidateStart candidateEnd, _) <- overlapping
                    , candidateStart < boundEnd
                    , candidateEnd > boundStart
                    ]
                start =
                    case starts of
                        [] -> skipLeadingTrivia src comments boundStart boundEnd
                        _ -> minimum starts
                end =
                    case ends of
                        [] -> skipTrailingTrivia src comments start boundEnd
                        _ -> maximum ends
                Span trimmedStart trimmedEnd = trimTrivia src comments (Span start end)
            if trimmedStart >= trimmedEnd
                then pure Nothing
                else do
                    let statementSpan = Span trimmedStart trimmedEnd
                        raw = BS.take (trimmedEnd - trimmedStart) (BS.drop trimmedStart src)
                    node <- nodeForSpan statementSpan overlapping
                    pure $
                        Just
                            StatementUnit
                                { unitStatement =
                                    Statement
                                        { stmtId = StatementId 0
                                        , stmtSpan = statementSpan
                                        , stmtRaw = raw
                                        }
                                , unitNode = node
                                }

        nodeForSpan statementSpan overlapping =
            pure $ snd <$> find ((== statementSpan) . fst) overlapping

spansBySegment :: [Span] -> [(Span, a)] -> [[(Span, a)]]
spansBySegment segments sortedSpans = go segments sortedSpans []
    where
        go [] _ _ = []
        go (Span start end : rest) remaining active =
            let (new, remaining') = span (\(Span candidateStart _, _) -> candidateStart < end) remaining
                active' =
                    filter
                        (\(Span _ candidateEnd, _) -> candidateEnd > start)
                        (active ++ new)
             in active' : go rest remaining' active'

statementSegments :: Int -> [Span] -> [Span]
statementSegments sourceLength = go 0 . sortOn spanStartByte
    where
        go cursor [] = [Span cursor sourceLength]
        go cursor (Span start end : rest)
            | start < cursor = go cursor rest
            | otherwise = Span cursor start : go end rest

trimTrivia :: ByteString -> [Span] -> Span -> Span
trimTrivia src comments (Span start end) =
    let trimmedStart = skipLeadingTrivia src comments start end
     in Span trimmedStart (skipTrailingTrivia src comments trimmedStart end)

skipLeadingTrivia :: ByteString -> [Span] -> Int -> Int -> Int
skipLeadingTrivia src comments = go
    where
        go position end
            | position >= end = end
            | isSQLSpace (BS.index src position) = go (position + 1) end
            | otherwise =
                case find (\(Span commentStart commentEnd) -> commentStart == position && commentEnd <= end) comments of
                    Just (Span _ commentEnd) -> go commentEnd end
                    Nothing -> position

skipTrailingTrivia :: ByteString -> [Span] -> Int -> Int -> Int
skipTrailingTrivia src comments = go
    where
        go start position
            | position <= start = start
            | isSQLSpace (BS.index src (position - 1)) = go start (position - 1)
            | otherwise =
                case find (\(Span commentStart commentEnd) -> commentStart >= start && commentEnd == position) comments of
                    Just (Span commentStart _) -> go start commentStart
                    Nothing -> position

isSQLSpace :: Word8 -> Bool
isSQLSpace byte = byte == 9 || byte == 10 || byte == 11 || byte == 12 || byte == 13 || byte == 32

extractStatements :: Tree -> ByteString -> IO [Statement]
extractStatements tree src = map unitStatement <$> extractStatementUnits tree src

-- | Parse SQL and count the number of top-level statements.
countStatements :: ByteString -> IO Int
countStatements sql = withParser $ \parser -> do
    parsed <- withParsedSQL parser sql $ \tree -> length <$> extractStatementUnits tree sql
    pure (fromMaybe 0 parsed)
