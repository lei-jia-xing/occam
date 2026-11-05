{-# LANGUAGE ForeignFunctionInterface #-}
module Occam.TreeSitter
    ( -- * Parser & Tree Lifecycle
      Parser
    , Tree
    , withParser
    , parseSQL
    , freeTree
    , withParsedSQL
      -- * Node Query
    , TSNode
    , rootNode
    , nodeType
    , nodeSpan
    , nodeChildren
    , nodeNamedChildren
    , nodeText
    , isNullNode
    , isNamedNode
    , cutSpan
    , cutNode
    , replaceSpan
    , replaceNode
      -- * Multi-Statement Slicing
    , extractStatements
    ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Foreign (Ptr, Word32, allocaBytes, nullPtr, castPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool(..), CUInt(..))
import Occam.Core (Span(..), Statement(..), StatementId(..))

-- ----------------------------------------------------------------------------
-- FFI Types & Declarations
-- ----------------------------------------------------------------------------

data C_TSParser
data C_TSTree

newtype Parser = Parser (Ptr C_TSParser)
newtype Tree   = Tree (Ptr C_TSTree)

-- TSNode is 32 bytes in C99 (uint32_t[4] + void* + void*)
newtype TSNode = TSNode ByteString
    deriving (Eq, Show)

foreign import ccall "occam_parser_new"
    c_parser_new :: IO (Ptr C_TSParser)

foreign import ccall "occam_parser_free"
    c_parser_free :: Ptr C_TSParser -> IO ()

foreign import ccall "occam_parse_sql"
    c_parse_sql :: Ptr C_TSParser -> CString -> CUInt -> IO (Ptr C_TSTree)

foreign import ccall "occam_tree_free"
    c_tree_free :: Ptr C_TSTree -> IO ()

foreign import ccall "occam_tree_root_p"
    c_tree_root_p :: Ptr C_TSTree -> Ptr () -> IO ()

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
rootNode (Tree t) = allocNode (\outPtr -> c_tree_root_p t outPtr)

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
    mapM (fetchChild p) [0 .. count - 1]
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
    filterM isNamedNode allCh
  where
    filterM _ [] = pure []
    filterM f (x:xs) = do
        flg <- f x
        rest <- filterM f xs
        pure (if flg then x : rest else rest)

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

extractStatements :: Tree -> ByteString -> IO [Statement]
extractStatements tree src = do
    root <- rootNode tree
    named <- nodeNamedChildren root
    -- In tree-sitter-sql, statements are named nodes under root (program)
    stmts <- filterM isStatement named
    mapM toStatement (zip [1..] stmts)
  where
    filterM _ [] = pure []
    filterM p (x:xs) = do
        cond <- p x
        rest <- filterM p xs
        pure (if cond then x : rest else rest)

    isStatement n = do
        t <- nodeType n
        pure (t /= "comment")

    toStatement (idx, n) = do
        sp@(Span start end) <- nodeSpan n
        let raw = BS.take (end - start) (BS.drop start src)
        pure $ Statement
            { stmtId   = StatementId idx
            , stmtSpan = sp
            , stmtRaw  = raw
            }
