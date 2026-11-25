-- | Bounded byte-prefix trie for deterministic SQL failing-prefix certificates.
module Occam.Certificate.PrefixCache (
    FailingPrefixCache,
    newFailingPrefixCache,
    recordFailingPrefix,
    checkFailingPrefix,
) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

data ByteTrie
    = TrieEmpty
    | TrieEdge !ByteString !ByteTrie
    | TrieBranch !Bool !(IntMap ByteTrie)
    deriving (Eq, Show)

matchTrie :: ByteTrie -> ByteString -> Bool
matchTrie TrieEmpty _ = False
matchTrie (TrieEdge edge child) candidate =
    let !edgeLen = BS.length edge
     in BS.length candidate >= edgeLen
            && BS.take edgeLen candidate == edge
            && matchTrie child (BS.drop edgeLen candidate)
matchTrie (TrieBranch isTerminal children) candidate
    | isTerminal = True
    | BS.null candidate = False
    | otherwise =
        let !b = fromIntegral (BS.head candidate)
         in case IntMap.lookup b children of
                Just child -> matchTrie child (BS.tail candidate)
                Nothing -> False

insertTrie :: ByteString -> ByteTrie -> (ByteTrie, Bool)
insertTrie bs TrieEmpty
    | BS.null bs = (TrieBranch True IntMap.empty, True)
    | otherwise = (TrieEdge bs (TrieBranch True IntMap.empty), True)
insertTrie bs (TrieEdge edge child)
    | bs == edge = insertExactEdge edge child
    | commonLen == BS.length edge =
        let (child', ins) = insertTrie (BS.drop commonLen bs) child
         in (TrieEdge edge child', ins)
    | commonLen == 0 =
        let !b1 = fromIntegral (BS.head bs)
            !b2 = fromIntegral (BS.head edge)
            !c1 = makeEdge (BS.tail bs) (TrieBranch True IntMap.empty)
            !c2 = makeEdge (BS.tail edge) child
         in (TrieBranch False (IntMap.fromList [(b1, c1), (b2, c2)]), True)
    | otherwise = splitEdgeAt commonLen bs edge child
    where
        !commonLen = commonPrefixLength bs edge
insertTrie bs (TrieBranch isTerm children)
    | BS.null bs = (TrieBranch True children, not isTerm)
    | otherwise =
        let !b = fromIntegral (BS.head bs)
            !rest = BS.tail bs
            (updatedChild, inserted) = case IntMap.lookup b children of
                Nothing -> (makeEdge rest (TrieBranch True IntMap.empty), True)
                Just existing -> insertTrie rest existing
         in (TrieBranch isTerm (IntMap.insert b updatedChild children), inserted)

insertExactEdge :: ByteString -> ByteTrie -> (ByteTrie, Bool)
insertExactEdge edge child = case child of
    TrieBranch True _ -> (TrieEdge edge child, False)
    TrieBranch False m -> (TrieEdge edge (TrieBranch True m), True)
    TrieEmpty -> (TrieEdge edge (TrieBranch True IntMap.empty), True)
    TrieEdge e _ ->
        let !b = fromIntegral (BS.head e)
         in (TrieEdge edge (TrieBranch True (IntMap.singleton b child)), True)

splitEdgeAt :: Int -> ByteString -> ByteString -> ByteTrie -> (ByteTrie, Bool)
splitEdgeAt commonLen bs edge child
    | BS.null bsRest = (TrieEdge common (TrieBranch True (IntMap.singleton b2 c2)), True)
    | otherwise =
        let !b1 = fromIntegral (BS.head bsRest)
            !c1 = makeEdge (BS.tail bsRest) (TrieBranch True IntMap.empty)
            !branch = TrieBranch False (IntMap.fromList [(b1, c1), (b2, c2)])
         in (TrieEdge common branch, True)
    where
        !common = BS.take commonLen edge
        !edgeRest = BS.drop commonLen edge
        !bsRest = BS.drop commonLen bs
        !b2 = fromIntegral (BS.head edgeRest)
        !c2 = makeEdge (BS.tail edgeRest) child

makeEdge :: ByteString -> ByteTrie -> ByteTrie
makeEdge rest c
    | BS.null rest = c
    | otherwise = TrieEdge rest c

commonPrefixLength :: ByteString -> ByteString -> Int
commonPrefixLength s1 s2 = go 0
    where
        !maxL = min (BS.length s1) (BS.length s2)
        go !i
            | i < maxL && BS.index s1 i == BS.index s2 i = go (i + 1)
            | otherwise = i

{- | Bounded, thread-safe memo of semicolon-terminated deterministic SQL error
prefixes observed during native result-oracle execution. Any future candidate
sharing an exact recorded byte prefix is guaranteed to encounter the same error
and fail the contract.
-}
data FailingPrefixCache = FailingPrefixCache
    { fpTrieRef :: !(IORef ByteTrie)
    , fpCountRef :: !(IORef Int)
    }

maxFailingPrefixes :: Int
maxFailingPrefixes = 512

newFailingPrefixCache :: IO FailingPrefixCache
newFailingPrefixCache = FailingPrefixCache <$> newIORef TrieEmpty <*> newIORef 0

recordFailingPrefix :: FailingPrefixCache -> ByteString -> IO ()
recordFailingPrefix FailingPrefixCache {..} prefix
    | BS.null prefix = pure ()
    | otherwise = do
        count <- readIORef fpCountRef
        if count >= maxFailingPrefixes
            then pure ()
            else do
                inserted <- atomicModifyIORef' fpTrieRef $ \trie ->
                    let (trie', ins) = insertTrie prefix trie
                     in (trie', ins)
                when inserted $
                    atomicModifyIORef' fpCountRef (\n -> (n + 1, ()))

checkFailingPrefix :: FailingPrefixCache -> ByteString -> IO Bool
checkFailingPrefix FailingPrefixCache {..} candidate = do
    trie <- readIORef fpTrieRef
    pure (matchTrie trie candidate)
