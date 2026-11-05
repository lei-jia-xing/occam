module Occam.Baseline (
    runSequentialDrop,
    runNaiveDdmin,
) where

import Data.ByteString (ByteString)
import Data.Set qualified as Set
import Occam.Core (Statement (..))
import Occam.Reducer (Oracle, cutRemovedStatements, partitionChunks)
import Occam.TreeSitter (extractStatements, parseSQL, withParser)

{- | Baseline 1 (Sequential Statement Deletion):
Loops through statements one by one, attempts removing statement i.
If Oracle is True, keeps it removed. Repeats until fixed-point.
-}
runSequentialDrop :: Oracle -> ByteString -> IO ByteString
runSequentialDrop oracle initialSQL = do
    initialOk <- oracle initialSQL
    if not initialOk
        then pure initialSQL
        else withParser $ \parser -> do
            mTree <- parseSQL parser initialSQL
            case mTree of
                Nothing -> pure initialSQL
                Just tree -> do
                    stmts <- extractStatements tree initialSQL
                    if length stmts <= 1
                        then pure initialSQL
                        else do
                            let fixedPoint currentStmts = do
                                    nextStmts <- dropPass currentStmts currentStmts
                                    if length nextStmts < length currentStmts
                                        then fixedPoint nextStmts
                                        else pure currentStmts

                                dropPass current [] = pure current
                                dropPass current (s : rest) =
                                    if not (any (\x -> stmtId x == stmtId s) current)
                                        then dropPass current rest
                                        else do
                                            let cand = filter (\x -> stmtId x /= stmtId s) current
                                            if null cand
                                                then dropPass current rest
                                                else do
                                                    let candSQL = cutRemovedStatements initialSQL stmts cand
                                                    ok <- oracle candSQL
                                                    if ok
                                                        then dropPass cand rest
                                                        else dropPass current rest

                            finalStmts <- fixedPoint stmts
                            pure $ cutRemovedStatements initialSQL stmts finalStmts

{- | Baseline 2 (Naive ddmin):
Standard Zeller delta-debugging on the statement list without dependency awareness
(splits statements into n chunks, tests complements/subsets, adjusts n).
-}
runNaiveDdmin :: Oracle -> ByteString -> IO ByteString
runNaiveDdmin oracle initialSQL = do
    initialOk <- oracle initialSQL
    if not initialOk
        then pure initialSQL
        else withParser $ \parser -> do
            mTree <- parseSQL parser initialSQL
            case mTree of
                Nothing -> pure initialSQL
                Just tree -> do
                    stmts <- extractStatements tree initialSQL
                    if length stmts <= 1
                        then pure initialSQL
                        else do
                            let ddmin currentStmts n
                                    | length currentStmts <= 1 = pure currentStmts
                                    | otherwise = do
                                        let chunks = partitionChunks n currentStmts
                                        mSubset <- trySubsets chunks
                                        case mSubset of
                                            Just reduced -> ddmin reduced 2
                                            Nothing -> do
                                                mComplement <- tryComplements chunks
                                                case mComplement of
                                                    Just reduced -> ddmin reduced (max 2 (n - 1))
                                                    Nothing ->
                                                        if n < length currentStmts
                                                            then ddmin currentStmts (min (length currentStmts) (2 * n))
                                                            else pure currentStmts
                                    where
                                        trySubsets [] = pure Nothing
                                        trySubsets (c : cs) =
                                            if not (null c) && length c < length currentStmts
                                                then do
                                                    let candSQL = cutRemovedStatements initialSQL stmts c
                                                    ok <- oracle candSQL
                                                    if ok
                                                        then pure (Just c)
                                                        else trySubsets cs
                                                else trySubsets cs

                                        tryComplements [] = pure Nothing
                                        tryComplements (c : cs) = do
                                            let cIds = Set.fromList (map stmtId c)
                                                comp = filter (\s -> not (Set.member (stmtId s) cIds)) currentStmts
                                            if not (null comp) && length comp < length currentStmts
                                                then do
                                                    let candSQL = cutRemovedStatements initialSQL stmts comp
                                                    ok <- oracle candSQL
                                                    if ok
                                                        then pure (Just comp)
                                                        else tryComplements cs
                                                else tryComplements cs

                            reducedStmts <- ddmin stmts 2
                            pure $ cutRemovedStatements initialSQL stmts reducedStmts
