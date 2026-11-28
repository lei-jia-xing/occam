-- | Statement closure checks, byte-span surgery, snapshots, and partitioning.
module Occam.Reducer.Statement (
    isValidClosure,
    isValidClosureFast,
    closeSet,
    closeSetFast,
    cutRemovedStatementIds,
    cutRemovedStatementInts,
    cutRemovedStatements,
    statementSQL,
    partitionChunks,
    partitionStatementsByBytes,
    partitionStatementsByWeights,
    statementSpanBytes,
    statementIds,
    statementIdsInt,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Occam.Core.Types (Span (..), Statement (..), StatementId (..))
import Occam.Dependency (DependencyGraph (..))
import Occam.Slicer (backwardSliceMany, backwardSliceManyFast)

-- | Check whether the retained statement set is closed under dependency predecessors.
isValidClosure :: DependencyGraph -> Set StatementId -> Bool
isValidClosure graph kept =
    let !keptInts = IntSet.fromList (map unStatementId (Set.toList kept))
     in isValidClosureFast graph keptInts

-- | Fast bitwise closure check directly over statement integers.
isValidClosureFast :: DependencyGraph -> IntSet.IntSet -> Bool
isValidClosureFast DependencyGraph {..} keptInts
    | IntMap.null graphFastEdges =
        let keptSet = Set.fromList (map StatementId (IntSet.toList keptInts))
         in all (isClosureOk keptSet) (Set.toList keptSet)
    | otherwise = all isClosureOkFast (IntSet.toList keptInts)
    where
        isClosureOk keptSet sid =
            let predecessors = Map.findWithDefault Set.empty sid graphEdges
             in predecessors `Set.isSubsetOf` keptSet

        isClosureOkFast sid =
            let predecessors = IntMap.findWithDefault IntSet.empty sid graphFastEdges
             in predecessors `IntSet.isSubsetOf` keptInts

-- | Transitive backward closure of a set of statement IDs.
closeSet :: DependencyGraph -> Set StatementId -> Set StatementId
closeSet graph roots = backwardSliceMany roots graph

-- | Transitive backward closure of a set of statement integers.
closeSetFast :: DependencyGraph -> IntSet.IntSet -> IntSet.IntSet
closeSetFast graph roots = backwardSliceManyFast roots graph

-- | Cut removed statements from the source SQL, preserving retained bytes and delimiters.
cutRemovedStatementIds :: ByteString -> [Statement] -> Set StatementId -> ByteString
cutRemovedStatementIds baseSQL allStatements keptIds =
    let !keptInts = IntSet.fromList (map unStatementId (Set.toList keptIds))
     in cutRemovedStatementInts baseSQL allStatements keptInts

-- | Fast statement byte surgery directly driven by an 'IntSet.IntSet' of retained statement IDs.
cutRemovedStatementInts :: ByteString -> [Statement] -> IntSet.IntSet -> ByteString
cutRemovedStatementInts baseSQL allStatements keptInts =
    case chunks 0 allStatements of
        [] -> BS.empty
        [single] -> single
        slices -> BS.concat slices
    where
        sourceLength = BS.length baseSQL

        chunks !cursor []
            | cursor < sourceLength = [BS.drop cursor baseSQL]
            | otherwise = []
        chunks !cursor (stmt : rest)
            | IntSet.member (unStatementId (stmtId stmt)) keptInts = chunks cursor rest
            | otherwise =
                let Span start _ = stmtSpan stmt
                    nextStart = case rest of
                        next : _ -> spanStartByte (stmtSpan next)
                        [] -> sourceLength
                    prefix =
                        if start > cursor
                            then (BS.take (start - cursor) (BS.drop cursor baseSQL) :)
                            else id
                 in prefix (chunks nextStart rest)

-- | Rebuild minimal SQL retaining @keptStatements@ (list wrapper over 'cutRemovedStatementInts').
cutRemovedStatements :: ByteString -> [Statement] -> [Statement] -> ByteString
cutRemovedStatements baseSQL allStatements keptStatements =
    cutRemovedStatementInts baseSQL allStatements (statementIdsInt keptStatements)

-- | Split a list into at most n approximately equal, contiguous chunks.
partitionChunks :: Int -> [a] -> [[a]]
partitionChunks n xs
    | n <= 1 = [xs]
    | null xs = []
    | otherwise = go n (length xs) xs
    where
        go _ _ [] = []
        go remaining total rest =
            let chunkSize = (total + remaining - 1) `div` remaining
                (chunk, next) = splitAt chunkSize rest
             in chunk : go (remaining - 1) (total - chunkSize) next

{- | Approximate equal-byte contiguous partitions. Reserve a statement for
each remaining group even when a single unusually large statement dominates.
-}
partitionStatementsByBytes :: Int -> [Statement] -> [[Statement]]
partitionStatementsByBytes groups statements =
    partitionStatementsByWeights
        groups
        [(s, statementSpanBytes s) | s <- statements]
        (sum (map statementSpanBytes statements))

-- | Partition statements into contiguous groups balanced by supplied integer weights.
partitionStatementsByWeights :: Int -> [(Statement, Int)] -> Int -> [[Statement]]
partitionStatementsByWeights groups statementsWithWeights totalBytes =
    go groups (length statementsWithWeights) totalBytes statementsWithWeights
    where
        go _ _ _ [] = []
        go remaining _ _ rest | remaining <= 1 = [map fst rest]
        go remaining count bytes ((firstStmt, firstWeight) : rest) =
            let target = (bytes + remaining - 1) `div` remaining
                (chunk, next, consumed, weight) =
                    consume target (count - remaining) [firstStmt] (1 :: Int) firstWeight rest
             in chunk : go (remaining - 1) (count - consumed) (bytes - weight) next
        consume target slots taken consumed weight ((nextStmt, nextWeight) : rest)
            | slots > 0
            , weight < target
            , abs (target - weight - nextWeight) <= target - weight =
                consume target (slots - 1) (nextStmt : taken) (consumed + 1) (weight + nextWeight) rest
        consume _ _ taken consumed weight rest = (reverse taken, rest, consumed, weight)

-- | UTF-8 byte span length covered by one statement.
statementSpanBytes :: Statement -> Int
statementSpanBytes statement =
    let Span start end = stmtSpan statement
     in end - start

-- | Collect statement IDs as a 'Set StatementId'.
statementIds :: [Statement] -> Set StatementId
statementIds = Set.fromList . map stmtId

-- | Collect statement IDs as a bitwise 'IntSet.IntSet'.
statementIdsInt :: [Statement] -> IntSet.IntSet
statementIdsInt = IntSet.fromList . map (unStatementId . stmtId)

statementSQL :: ByteString -> [Statement] -> [Statement] -> ByteString
statementSQL = cutRemovedStatements
