module Occam.Reducer (
    Oracle,
    isValidClosure,
    closeSet,
    closureDdmin,
    cutRemovedStatements,
    partitionChunks,
    pruneClauses,
    simplifyExpressions,
    reduceSQL,
    reduceSQLWithLogger,
) where

import Control.Monad (when)
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Occam.Core (Span (..), Statement (..), StatementId (..))
import Occam.Dependency (DependencyGraph (..), analyzeStatements)
import Occam.Logger (Logger, logDebug, logDebug', logInfo', logWarn, nullLogger)
import Occam.Slicer (backwardSlice)
import Occam.TreeSitter (
    Parser,
    collectDescendants,
    cutSpan,
    extractStatementNodes,
    nodeNamedChildren,
    nodeSpan,
    nodeText,
    nodeType,
    parseSQL,
    replaceSpan,
    rootNode,
    withParsedSQL,
    withParser,
 )

-- | Oracle returns True if candidate SQL triggers the crash.
type Oracle = ByteString -> IO Bool

{- | A retained candidate set of StatementIds K is valid iff
forall s in K, graphEdges(s) is subset of K (no dangling references to deleted definitions).
-}
isValidClosure :: DependencyGraph -> Set StatementId -> Bool
isValidClosure DependencyGraph {..} kept =
    all isClosureOk (Set.toList kept)
    where
        isClosureOk s =
            let preds = Map.findWithDefault Set.empty s graphEdges
             in preds `Set.isSubsetOf` kept

-- | Transitive backward closure of a set of StatementIds.
closeSet :: DependencyGraph -> Set StatementId -> Set StatementId
closeSet graph sids =
    Set.unions [backwardSlice sid graph | sid <- Set.toList sids]

-- | Cut all removed statements from baseSQL without re-parsing/unparsing.
cutRemovedStatements :: ByteString -> [Statement] -> [Statement] -> ByteString
cutRemovedStatements baseSQL allStmts keptStmts =
    cutSpans (map (absorbTrailing baseSQL) removedSpans) baseSQL
    where
        keptIds = Set.fromList (map stmtId keptStmts)
        removedStmts = filter (\s -> not (Set.member (stmtId s) keptIds)) allStmts
        removedSpans = map stmtSpan removedStmts

absorbTrailing :: ByteString -> Span -> Span
absorbTrailing src (Span s e) =
    let len = BS.length src
        e1 = if e < len && BS.index src e == 59 then e + 1 else e -- ';'
        e2 = if e1 < len && BS.index src e1 == 10 then e1 + 1 else e1 -- '\n'
     in Span s e2

cutSpans :: [Span] -> ByteString -> ByteString
cutSpans spans src =
    foldl' (\acc (Span s e) -> cutSpan s e acc) src sortedSpans
    where
        sortedSpans = sortOn (Down . spanStartByte) spans

{- | Level 2: Closure-Constrained Delta Debugging.
Runs ddmin partitions, but closes candidate subsets using the dependency graph before calling the Oracle.
-}
closureDdmin :: Oracle -> ByteString -> [Statement] -> DependencyGraph -> IO [Statement]
closureDdmin oracle baseSQL initialStmts graph
    | length initialStmts <= 1 = pure initialStmts
    | otherwise = go initialStmts 2
    where
        go currentStmts n
            | length currentStmts <= 1 = pure currentStmts
            | otherwise = do
                let chunks = partitionChunks n currentStmts
                mSubset <- trySubsets currentStmts chunks
                case mSubset of
                    Just reduced -> go reduced 2
                    Nothing -> do
                        mComplement <- tryComplements currentStmts chunks
                        case mComplement of
                            Just reduced -> go reduced (max 2 (n - 1))
                            Nothing ->
                                if n < length currentStmts
                                    then go currentStmts (min (length currentStmts) (2 * n))
                                    else pure currentStmts

        trySubsets _ [] = pure Nothing
        trySubsets currentStmts (c : cs) = do
            let keptIds = closeSet graph (Set.fromList (map stmtId c))
                candidate = filter (\s -> Set.member (stmtId s) keptIds) currentStmts
            if not (null candidate)
                && length candidate < length currentStmts
                && isValidClosure graph (Set.fromList (map stmtId candidate))
                then do
                    let candSQL = cutRemovedStatements baseSQL initialStmts candidate
                    ok <- oracle candSQL
                    if ok
                        then pure (Just candidate)
                        else trySubsets currentStmts cs
                else trySubsets currentStmts cs

        tryComplements _ [] = pure Nothing
        tryComplements currentStmts (c : cs) = do
            let comp = filter (\s -> not (Set.member (stmtId s) (Set.fromList (map stmtId c)))) currentStmts
                keptIds = closeSet graph (Set.fromList (map stmtId comp))
                candidate = filter (\s -> Set.member (stmtId s) keptIds) currentStmts
            if not (null candidate)
                && length candidate < length currentStmts
                && isValidClosure graph (Set.fromList (map stmtId candidate))
                then do
                    let candSQL = cutRemovedStatements baseSQL initialStmts candidate
                    ok <- oracle candSQL
                    if ok
                        then pure (Just candidate)
                        else tryComplements currentStmts cs
                else tryComplements currentStmts cs

partitionChunks :: Int -> [a] -> [[a]]
partitionChunks n xs
    | n <= 1 = [xs]
    | null xs = []
    | otherwise =
        let len = length xs
            chunkSize = (len + n - 1) `div` n
            (h, t) = splitAt chunkSize xs
         in h : partitionChunks (n - 1) t

-- | Level 1 helper: find the crashing statement (earliest prefix triggering oracle).
findCrashStatement :: Oracle -> ByteString -> [Statement] -> IO Statement
findCrashStatement oracle baseSQL stmts =
    case reverse stmts of
        [] -> fail "findCrashStatement: empty statement list"
        (fallback : _) -> go [1 .. length stmts] fallback
    where
        go [] fallback = pure fallback
        go (i : is) fallback = do
            let pref = take i stmts
                candSQL = cutRemovedStatements baseSQL stmts pref
            ok <- oracle candSQL
            if ok
                then case reverse pref of
                    (p : _) -> pure p
                    [] -> go is fallback
                else go is fallback

-- | Level 3: Intra-statement clause pruning (WHERE, ORDER BY, GROUP BY, HAVING, LIMIT).
pruneClauses :: Oracle -> ByteString -> IO ByteString
pruneClauses oracle sql = withParser $ \parser -> pruneClausesWithParser oracle parser sql

pruneClausesWithParser :: Oracle -> Parser -> ByteString -> IO ByteString
pruneClausesWithParser oracle parser = go
    where
        go currentSQL = do
            mTree <- parseSQL parser currentSQL
            case mTree of
                Nothing -> pure currentSQL
                Just tree -> do
                    root <- rootNode tree
                    clauseNodes <- collectDescendants isClauseNode root
                    tryRemove currentSQL clauseNodes

        isClauseNode t =
            t `elem` ["where", "order_by", "group_by", "having", "limit"]

        tryRemove currentSQL [] = pure currentSQL
        tryRemove currentSQL (n : ns) = do
            Span start end <- nodeSpan n
            let start' =
                    if start > 0 && BS.index currentSQL (start - 1) == 32
                        then start - 1
                        else start
            let candidate = cutSpan start' end currentSQL
            ok <- oracle candidate
            if ok
                then go candidate
                else tryRemove currentSQL ns

-- | Fixed-Point Reducer Loop: Level 1 -> Level 2 -> Level 3 until fixed point.
reduceSQL :: Oracle -> ByteString -> IO ByteString
reduceSQL = reduceSQLWithLogger nullLogger

-- | Fixed-Point Reducer Loop with structured progress logging.
reduceSQLWithLogger :: Logger -> Oracle -> ByteString -> IO ByteString
reduceSQLWithLogger logger oracle initialSQL = do
    initialOk <- oracle initialSQL
    if not initialOk
        then do
            logWarn logger "occam::reducer" "Initial SQL candidate failed oracle check"
            pure initialSQL
        else loop (1 :: Int) initialSQL
    where
        loop iter currentSQL = do
            logInfo'
                logger
                "occam::reducer"
                ("Starting reduction iteration " <> T.pack (show iter) <> "...")
                [("iteration", toJSON iter), ("bytes", toJSON (BS.length currentSQL))]
            nextSQL <- reduceIterationWithLogger logger oracle currentSQL
            if BS.length nextSQL < BS.length currentSQL
                then do
                    logInfo'
                        logger
                        "occam::reducer"
                        ("Iteration " <> T.pack (show iter) <> " reduced SQL, continuing...")
                        [("bytes_before", toJSON (BS.length currentSQL)), ("bytes_after", toJSON (BS.length nextSQL))]
                    loop (iter + 1) nextSQL
                else do
                    logInfo'
                        logger
                        "occam::reducer"
                        "Fixed point reached: no further reduction possible"
                        [("total_iterations", toJSON iter), ("final_bytes", toJSON (BS.length currentSQL))]
                    pure currentSQL

reduceIterationWithLogger :: Logger -> Oracle -> ByteString -> IO ByteString
reduceIterationWithLogger logger oracle sql = withParser $ \parser -> do
    mTree <- parseSQL parser sql
    case mTree of
        Nothing -> pure sql
        Just tree -> do
            pairs <- extractStatementNodes tree sql
            if null pairs
                then pure sql
                else do
                    let stmts = map fst pairs
                        indexed = [(stmtId s, n) | (s, n) <- pairs]
                    graph <- analyzeStatements indexed sql

                    -- Level 1: Backward slice on crash statement
                    crashStmt <- findCrashStatement oracle sql stmts
                    let StatementId cid = stmtId crashStmt
                    logDebug'
                        logger
                        "occam::reducer"
                        "Level 1: isolated earliest crash statement"
                        [("crash_stmt_id", toJSON cid)]
                    let slice = backwardSlice (stmtId crashStmt) graph
                        l1Stmts = filter (\s -> Set.member (stmtId s) slice) stmts
                    sql1 <-
                        if length l1Stmts < length stmts
                            then do
                                let cand = cutRemovedStatements sql stmts l1Stmts
                                ok <- oracle cand
                                if ok
                                    then do
                                        logInfo'
                                            logger
                                            "occam::reducer"
                                            "Level 1 (Backward Slicing): pruned unreferenced statements"
                                            [ ("statements_before", toJSON (length stmts))
                                            , ("statements_after", toJSON (length l1Stmts))
                                            , ("bytes", toJSON (BS.length cand))
                                            ]
                                        pure cand
                                    else pure sql
                            else do
                                logDebug logger "occam::reducer" "Level 1 (Backward Slicing): all statements required by slice"
                                pure sql

                    -- Level 2: Closure-constrained delta debugging
                    sql2 <- withParsedSQL parser sql1 $ \tree1 -> do
                        pairs1 <- extractStatementNodes tree1 sql1
                        let stmts1 = map fst pairs1
                            indexed1 = [(stmtId s, n) | (s, n) <- pairs1]
                        graph1 <- analyzeStatements indexed1 sql1
                        logDebug logger "occam::reducer" "Level 2: running closure-constrained ddmin..."
                        l2Stmts <- closureDdmin oracle sql1 stmts1 graph1
                        if length l2Stmts < length stmts1
                            then do
                                let cand = cutRemovedStatements sql1 stmts1 l2Stmts
                                ok <- oracle cand
                                if ok
                                    then do
                                        logInfo'
                                            logger
                                            "occam::reducer"
                                            "Level 2 (Closure ddmin): reduced statements"
                                            [ ("statements_before", toJSON (length stmts1))
                                            , ("statements_after", toJSON (length l2Stmts))
                                            , ("bytes", toJSON (BS.length cand))
                                            ]
                                        pure cand
                                    else pure sql1
                            else pure sql1
                    let sql2Final = fromMaybe sql1 sql2

                    -- Level 3: Intra-statement clause pruning
                    logDebug logger "occam::reducer" "Level 3: running clause pruning..."
                    sql3 <- pruneClausesWithParser oracle parser sql2Final
                    when (BS.length sql3 < BS.length sql2Final) $
                        logInfo'
                            logger
                            "occam::reducer"
                            "Level 3 (Clause Pruning): pruned optional clauses"
                            [ ("bytes_before", toJSON (BS.length sql2Final))
                            , ("bytes_after", toJSON (BS.length sql3))
                            ]

                    -- Level 4: Intra-statement expression simplification
                    logDebug logger "occam::reducer" "Level 4: running expression simplification..."
                    sql4 <- simplifyExpressionsWithParser oracle parser sql3
                    when (BS.length sql4 < BS.length sql3) $
                        logInfo'
                            logger
                            "occam::reducer"
                            "Level 4 (Expression Simplification): simplified expressions"
                            [ ("bytes_before", toJSON (BS.length sql3))
                            , ("bytes_after", toJSON (BS.length sql4))
                            ]
                    pure sql4

-- | Level 4: Intra-statement expression simplification (binary, unary, parenthesized).
simplifyExpressions :: Oracle -> ByteString -> IO ByteString
simplifyExpressions oracle sql = withParser $ \parser -> simplifyExpressionsWithParser oracle parser sql

simplifyExpressionsWithParser :: Oracle -> Parser -> ByteString -> IO ByteString
simplifyExpressionsWithParser oracle parser = go
    where
        go currentSQL = do
            mTree <- parseSQL parser currentSQL
            case mTree of
                Nothing -> pure currentSQL
                Just tree -> do
                    root <- rootNode tree
                    exprNodes <- collectDescendants isSimplifiable root
                    trySimplify currentSQL exprNodes

        isSimplifiable t =
            t == "binary_expression"
                || t == "unary_expression"
                || t == "parenthesized_expression"

        trySimplify currentSQL [] = pure currentSQL
        trySimplify currentSQL (node : rest) = do
            t <- nodeType node
            Span start end <- nodeSpan node
            ch <- nodeNamedChildren node
            replacements <- getReplacements currentSQL t ch
            mSuccess <- tryReplacements currentSQL start end replacements
            case mSuccess of
                Just newSQL -> go newSQL
                Nothing -> trySimplify currentSQL rest

        getReplacements currentSQL t ch =
            case (t, ch) of
                ("binary_expression", left : rest@(_ : _)) -> do
                    leftText <- nodeText currentSQL left
                    rightText <- nodeText currentSQL (last rest)
                    pure [leftText, rightText, "0", "1", "'0'"]
                ("unary_expression", op : _) -> do
                    opText <- nodeText currentSQL op
                    pure [opText, "0", "1", "'0'"]
                ("parenthesized_expression", inner : _) -> do
                    innerText <- nodeText currentSQL inner
                    pure [innerText]
                _ -> pure []

        tryReplacements _ _ _ [] = pure Nothing
        tryReplacements currentSQL start end (rep : reps) =
            if BS.length rep < (end - start)
                then do
                    let candidate = replaceSpan start end rep currentSQL
                    ok <- oracle candidate
                    if ok
                        then pure (Just candidate)
                        else tryReplacements currentSQL start end reps
                else tryReplacements currentSQL start end reps
