-- | Provably sound early-rejection certificates for native result oracles.
module Occam.Certificate (
    EarlyRejectCertifier (..),
    EarlyRejectStats (..),
    EarlyRejectTracker,
    FailingPrefixCache,
    newFailingPrefixCache,
    recordFailingPrefix,
    checkFailingPrefix,
    emptyEarlyRejectCertifier,
    emptyEarlyRejectStats,
    buildEarlyRejectCertifier,
    checkEarlyRejection,
    checkEarlyRejectionIds,
    checkEarlyRejectionBytes,
    checkEarlyRejectionSyntaxEdit,
    newEarlyRejectTracker,
    newEarlyRejectTrackerWithFailures,
    readEarlyRejectStats,
    recordEarlyRejectBuildNanos,
    checkEarlyRejectionIdsTracked,
    checkEarlyRejectionBytesTracked,
    checkEarlyRejectionSyntaxEditTracked,
) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64, Word8)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (
    BarrierReason (..),
    BarrierScope (..),
    CatalogOp (..),
    DataWrite (..),
    EarlyRejectReason (..),
    Ident (..),
    Name (..),
    NameRef (..),
    ReadFact (..),
    ReadMode (..),
    RenameOp (..),
    ResultTrace (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    SequenceDef (..),
    Span (..),
    Statement (..),
    StatementId (..),
    TableDef (..),
    TxEffect (..),
    ViewDef (..),
 )
import Occam.Facts (ParserEdit (..), StatementFacts (..), StatementUnit (..))

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

-- | Precomputed early-rejection certificates anchored to one accepted base SQL script.
data EarlyRejectCertifier = EarlyRejectCertifier
    { certEnabled :: !Bool
    , certContiguousIds :: !Bool
    , certBaseSQL :: !ByteString
    , certExactPrefixByteLens :: !IntSet
    , certExactPrefixLengths :: !IntSet
    , certNonResultReady :: !Bool
    , certGuaranteedNonResult :: !IntSet
    , certMissingDefReqs :: !(IntMap [IntSet])
    , certUniqueDefiners :: !(IntMap IntSet)
    , certProtectedResultIndex :: !(Maybe (Int, Int))
    {- ^ @(expectedUnitCount, lastResultIndex)@ when every trailing unit
    strictly after @lastResultIndex@ satisfies 'unitCannotProduceResult'.
    -}
    }
    deriving (Eq, Show)

emptyEarlyRejectCertifier :: EarlyRejectCertifier
emptyEarlyRejectCertifier =
    EarlyRejectCertifier
        { certEnabled = False
        , certContiguousIds = False
        , certBaseSQL = BS.empty
        , certExactPrefixByteLens = IntSet.empty
        , certExactPrefixLengths = IntSet.empty
        , certNonResultReady = False
        , certGuaranteedNonResult = IntSet.empty
        , certMissingDefReqs = IntMap.empty
        , certUniqueDefiners = IntMap.empty
        , certProtectedResultIndex = Nothing
        }

-- | Per-stage certificate check and hit accounting, including checking overhead.
data EarlyRejectStats = EarlyRejectStats
    { earlyRejectChecked :: !Int
    , earlyRejectExactPrefix :: !Int
    , earlyRejectNoResult :: !Int
    , earlyRejectFailingPrefix :: !Int
    , earlyRejectMissingDef :: !Int
    , earlyRejectColumnArity :: !Int
    , earlyRejectCheckNanos :: !Word64
    }
    deriving (Eq, Show)

emptyEarlyRejectStats :: EarlyRejectStats
emptyEarlyRejectStats = EarlyRejectStats 0 0 0 0 0 0 0

data EarlyRejectTracker = EarlyRejectTracker
    { trackerStatsRef :: !(IORef EarlyRejectStats)
    , trackerFailures :: !(Maybe FailingPrefixCache)
    }

newEarlyRejectTracker :: IO EarlyRejectTracker
newEarlyRejectTracker = newEarlyRejectTrackerWithFailures Nothing

newEarlyRejectTrackerWithFailures :: Maybe FailingPrefixCache -> IO EarlyRejectTracker
newEarlyRejectTrackerWithFailures mFailures =
    EarlyRejectTracker <$> newIORef emptyEarlyRejectStats <*> pure mFailures

readEarlyRejectStats :: EarlyRejectTracker -> IO EarlyRejectStats
readEarlyRejectStats = readIORef . trackerStatsRef

recordEarlyRejectBuildNanos :: EarlyRejectTracker -> Word64 -> IO ()
recordEarlyRejectBuildNanos tracker nanos =
    modifyIORef'
        (trackerStatsRef tracker)
        (\stats -> stats {earlyRejectCheckNanos = earlyRejectCheckNanos stats + nanos})

{- | Build early-rejection certificates from an accepted script, its parsed
statement units, and native engine trace evidence.

Four static certificate classes are derived (with 'EarlyRejectFailingPrefix'
tracked dynamically per native run):

1. 'EarlyRejectExactPrefix': When the native run was deterministic and recorded
   exact engine statement end byte offsets, any proper prefix @0..k@ whose
   latest result did not match the contract is certified rejected. Trailing
   ASCII-whitespace-only padding up to the next statement start is also
   included so exact byte slices from statement cutting match directly.

2. 'EarlyRejectNoResultStatement': When engine and parser statement counts agree
   with zero parse errors and the script produced a required result set, any
   subsequence whose retained statements all carry the strict AST
   @unitCannotProduceResult@ allowlist bit and produced no result in the native
   trace cannot produce a result set and is certified rejected.

3. 'EarlyRejectMissingDefinition': When engine and parser statement counts agree
   with zero parse errors, any retained statement that executes a direct read or
   write against a script-defined relation (excluding views/triggers/drops) or
   a transaction/savepoint terminator whose prior definers/openers (together
   with any preceding opaque barrier statements) are all absent is guaranteed to
   fail with a SQL error and is certified rejected.

4. 'EarlyRejectColumnArityMismatch': For intra-statement reduction on a clean
   script whose final result statement is followed exclusively by
   @unitCannotProduceResult@ statements, any edit marked
   'parserEditTopProjection' on that result statement strictly reduces output
   column count and is certified rejected.
-}
buildEarlyRejectCertifier ::
    ByteString -> [StatementUnit] -> Maybe ResultTrace -> EarlyRejectCertifier
buildEarlyRejectCertifier _ _ Nothing = emptyEarlyRejectCertifier
buildEarlyRejectCertifier baseSQL units (Just trace) =
    EarlyRejectCertifier
        { certEnabled = enabled
        , certContiguousIds = contiguousIds
        , certBaseSQL = if IntSet.null prefixByteLens then BS.empty else baseSQL
        , certExactPrefixByteLens = prefixByteLens
        , certExactPrefixLengths = prefixLengths
        , certNonResultReady = nonResultReady
        , certGuaranteedNonResult = guaranteedNonResult
        , certMissingDefReqs = missingDefReqs
        , certUniqueDefiners = uniqueDefiners
        , certProtectedResultIndex = protectedResultIndex
        }
    where
        totalStmts = traceStatementCount trace
        unitCount = length units
        baseLen = BS.length baseSQL
        unitIds = map (unStatementId . stmtId . unitStatement) units
        countsMatch = totalStmts == unitCount
        contiguousIds = countsMatch && unitIds == [1 .. unitCount]
        nextStarts =
            map (Just . spanStartByte . stmtSpan . unitStatement) (drop 1 units)
                ++ repeat Nothing

        uniqueDefiners = IntMap.foldlWithKey' addUniqueDefiners IntMap.empty missingDefReqs
        addUniqueDefiners acc sid = foldl' (addUniqueDefiner sid) acc
        addUniqueDefiner sid m req
            | Just (defSid, rest) <- IntSet.minView req
            , IntSet.null rest
            , defSid /= sid =
                IntMap.insertWith IntSet.union defSid (IntSet.singleton sid) m
            | otherwise = m

        (prefixByteLens, prefixLengths) = case traceEndByteOffsets trace of
            Just endOffsets
                | traceDeterministic trace
                , totalStmts >= 2
                , length endOffsets == totalStmts ->
                    collectPrefixes (zip3 [0 :: Int ..] endOffsets nextStarts) IntSet.empty IntSet.empty
            _ -> (IntSet.empty, IntSet.empty)

        collectPrefixes [] !byteLensAcc !lensAcc = (byteLensAcc, lensAcc)
        collectPrefixes ((k, endByte, mNextStart) : rest) !byteLensAcc !lensAcc
            | k >= totalStmts - 1 = collectPrefixes rest byteLensAcc lensAcc
            | endByte < 0 || endByte > baseLen = collectPrefixes rest byteLensAcc lensAcc
            | IntSet.member k (tracePrefixMatchIndices trace) = collectPrefixes rest byteLensAcc lensAcc
            | otherwise =
                let !byteLens1 = IntSet.insert endByte byteLensAcc
                    (!byteLens2, !lens2) = extendWithWhitespace k endByte mNextStart byteLens1 lensAcc
                 in collectPrefixes rest byteLens2 lens2

        extendWithWhitespace k endByte (Just nextStart) !byteLens1 !lensAcc
            | countsMatch
            , endByte <= nextStart
            , nextStart <= baseLen
            , isAsciiWhitespace (BS.take (nextStart - endByte) (BS.drop endByte baseSQL)) =
                ( IntSet.insert nextStart byteLens1
                , if contiguousIds then IntSet.insert (k + 1) lensAcc else lensAcc
                )
        extendWithWhitespace _ _ _ !byteLens1 !lensAcc = (byteLens1, lensAcc)

        nonResultReady =
            countsMatch
                && traceLastResultIndex trace >= 0
                && not (null units)
                && all (isNothing . unitParseError) units

        guaranteedNonResult
            | not nonResultReady = IntSet.empty
            | otherwise =
                IntSet.fromList
                    [ unStatementId (stmtId (unitStatement unit))
                    | (idx, unit) <- zip [0 :: Int ..] units
                    , unitCannotProduceResult unit
                    , IntSet.notMember idx (traceResultIndices trace)
                    ]

        missingDefReqs
            | nonResultReady && all (isJust . unitFacts) units = buildMissingDefReqs units
            | otherwise = IntMap.empty

        protectedResultIndex
            | nonResultReady
            , let lastIdx = traceLastResultIndex trace
            , lastIdx >= 0 && lastIdx < unitCount
            , all unitCannotProduceResult (drop (lastIdx + 1) units) =
                Just (unitCount, lastIdx)
            | otherwise = Nothing

        enabled =
            not (IntSet.null prefixByteLens)
                || nonResultReady
                || not (IntMap.null missingDefReqs)
                || isJust protectedResultIndex

data MissingDefAcc = MissingDefAcc
    { mdaRelDefs :: !(Map Text IntSet)
    , mdaTxOpens :: !IntSet
    , mdaSpOpens :: !(Map Text IntSet)
    , mdaOpaqueDdl :: !IntSet
    , mdaOpaqueTx :: !IntSet
    , mdaReqs :: !(IntMap [IntSet])
    }

-- | Precompute per-statement required prior definers/openers in source order.
buildMissingDefReqs :: [StatementUnit] -> IntMap [IntSet]
buildMissingDefReqs =
    mdaReqs . foldl' stepMissingDefUnit emptyAcc
    where
        emptyAcc = MissingDefAcc Map.empty IntSet.empty Map.empty IntSet.empty IntSet.empty IntMap.empty

stepMissingDefUnit :: MissingDefAcc -> StatementUnit -> MissingDefAcc
stepMissingDefUnit acc unit = case unitFacts unit of
    Nothing ->
        acc
            { mdaOpaqueDdl = IntSet.insert sid (mdaOpaqueDdl acc)
            , mdaOpaqueTx = IntSet.insert sid (mdaOpaqueTx acc)
            }
    Just facts -> stepMissingDefFacts sid acc facts
    where
        !sid = unStatementId (stmtId (unitStatement unit))

stepMissingDefFacts :: Int -> MissingDefAcc -> StatementFacts -> MissingDefAcc
stepMissingDefFacts sid acc facts =
    MissingDefAcc
        { mdaRelDefs = relDefs'
        , mdaTxOpens = txOpens'
        , mdaSpOpens = spOpens'
        , mdaOpaqueDdl =
            if any isOpaqueDdlBarrier barriers then IntSet.insert sid (mdaOpaqueDdl acc) else mdaOpaqueDdl acc
        , mdaOpaqueTx =
            if any isOpaqueTxBarrier barriers then IntSet.insert sid (mdaOpaqueTx acc) else mdaOpaqueTx acc
        , mdaReqs = if null allReqs then mdaReqs acc else IntMap.insert sid allReqs (mdaReqs acc)
        }
    where
        sem = factSemantics facts
        ops = factOps sem
        barriers = factBarriers sem
        txs = factPreTx sem ++ factPostTx sem
        -- Never require referenced relations inside deferred view/trigger DDL or DROP.
        skipRelReqs = any isDeferredOrDropOp ops
        selfDefined = Set.fromList (concatMap opDefinedRelNames ops)
        refRels
            | skipRelReqs = []
            | otherwise =
                Set.toList (Set.fromList (concatMap opReferencedRelNames ops) `Set.difference` selfDefined)
        relReqs =
            [ IntSet.union defs (mdaOpaqueDdl acc)
            | rel <- refRels
            , Just defs <- [Map.lookup rel (mdaRelDefs acc)]
            ]
        txReqs =
            [ req
            | tx <- txs
            , Just req <- [txRequirement tx (mdaTxOpens acc) (mdaSpOpens acc) (mdaOpaqueTx acc)]
            ]
        allReqs = dedupIntSetsSmall (relReqs ++ txReqs)
        relDefs' =
            foldl'
                (\m rel -> Map.insertWith IntSet.union rel (IntSet.singleton sid) m)
                (mdaRelDefs acc)
                (Set.toList selfDefined)
        (txOpens', spOpens') = foldl' (recordTxOpener sid) (mdaTxOpens acc, mdaSpOpens acc) txs

isDeferredOrDropOp :: CatalogOp -> Bool
isDeferredOrDropOp = \case
    DefineTableOp _ -> True
    DefineViewOp _ -> True
    DefineTriggerOp _ -> True
    DefineIndexOp _ -> True
    DropCatalogOp _ -> True
    AlterTableOp _ -> True
    _ -> False

baseRelName :: Name -> Text
baseRelName (Name []) = T.empty
baseRelName (Name parts) = T.toLower (identText (lastPart parts))
    where
        lastPart [x] = x
        lastPart (_ : xs) = lastPart xs
        lastPart [] = Ident T.empty

opDefinedRelNames :: CatalogOp -> [Text]
opDefinedRelNames = \case
    DefineTableOp td -> [baseRelName (tableName td)]
    DefineViewOp vd -> [baseRelName (viewName vd)]
    DefineSequenceOp sd -> [baseRelName (sequenceName sd)]
    RenameOp (RenameRelation _ newName _) -> [baseRelName newName]
    _ -> []

opReferencedRelNames :: CatalogOp -> [Text]
opReferencedRelNames = \case
    ReadOp rf
        | readMode rf /= ReadSchema -> [baseRelName (nameRefName (readSource rf))]
    WriteDataOp dw ->
        baseRelName (nameRefName (dataWriteTarget dw))
            : [baseRelName (nameRefName (readSource rf)) | rf <- dataWriteReads dw, readMode rf /= ReadSchema]
    _ -> []

txRequirement :: TxEffect -> IntSet -> Map Text IntSet -> IntSet -> Maybe IntSet
txRequirement tx txOpens spOpens opaqueTx = case tx of
    TxCommit _
        | not (IntSet.null txOpens) -> Just (IntSet.union txOpens opaqueTx)
    TxRollbackAll
        | not (IntSet.null txOpens) -> Just (IntSet.union txOpens opaqueTx)
    TxRollbackTo sp ->
        IntSet.union opaqueTx <$> Map.lookup (T.toLower (identText sp)) spOpens
    TxReleaseSavepoint sp ->
        IntSet.union opaqueTx <$> Map.lookup (T.toLower (identText sp)) spOpens
    _ -> Nothing

recordTxOpener :: Int -> (IntSet, Map Text IntSet) -> TxEffect -> (IntSet, Map Text IntSet)
recordTxOpener sid (txOpens, spOpens) = \case
    TxBegin _ -> (IntSet.insert sid txOpens, spOpens)
    TxSavepoint sp ->
        let spKey = T.toLower (identText sp)
         in (IntSet.insert sid txOpens, Map.insertWith IntSet.union spKey (IntSet.singleton sid) spOpens)
    _ -> (txOpens, spOpens)

isOpaqueDdlBarrier :: ScopedBarrier -> Bool
isOpaqueDdlBarrier sb =
    barrierScope sb == GlobalScope
        || case barrierReason sb of
            SyntaxErrorBarrier -> True
            UnknownStatementBarrier -> True
            UnknownRelationBarrier -> True
            UnknownDDLBarrier -> True
            UnknownDMLBarrier -> True
            UnknownAlterBarrier -> True
            UnknownViewBarrier -> True
            UnknownSessionEffectBarrier -> True
            UnsupportedTriggerBodyBarrier -> True
            OpaqueTriggerBodyBarrier -> True
            _ -> False

isOpaqueTxBarrier :: ScopedBarrier -> Bool
isOpaqueTxBarrier sb = case barrierReason sb of
    SyntaxErrorBarrier -> True
    UnknownStatementBarrier -> True
    UnknownSessionEffectBarrier -> True
    UnsupportedTransactionBarrier -> True
    _ -> False

dedupIntSetsSmall :: [IntSet] -> [IntSet]
dedupIntSetsSmall [] = []
dedupIntSetsSmall (x : xs) = x : dedupIntSetsSmall (filter (/= x) xs)

isAsciiWhitespace :: ByteString -> Bool
isAsciiWhitespace = BS.all isWsByte
    where
        isWsByte :: Word8 -> Bool
        isWsByte b = b == 0x20 || b == 0x09 || b == 0x0a || b == 0x0d

-- | Fast certificate check over retained statement IDs before rendering SQL bytes.
checkEarlyRejectionIds :: EarlyRejectCertifier -> IntSet -> Maybe EarlyRejectReason
checkEarlyRejectionIds cert keptInts
    | not (certEnabled cert) = Nothing
    | certContiguousIds cert
    , not (IntSet.null keptInts)
    , IntSet.findMin keptInts == 1
    , let !prefixLen = IntSet.size keptInts
    , IntSet.findMax keptInts == prefixLen
    , IntSet.member prefixLen (certExactPrefixLengths cert) =
        Just EarlyRejectExactPrefix
    | certNonResultReady cert
    , keptInts `IntSet.isSubsetOf` certGuaranteedNonResult cert =
        Just EarlyRejectNoResultStatement
    | not (IntMap.null (certMissingDefReqs cert))
    , hasMissingDefinition cert keptInts =
        Just EarlyRejectMissingDefinition
    | otherwise = Nothing
    where
        hasMissingDefinition c kept =
            anyMissingUniqueDef || IntSet.foldr checkSid False kept
            where
                anyMissingUniqueDef =
                    IntMap.foldrWithKey'
                        ( \defSid dependents acc ->
                            acc || (IntSet.notMember defSid kept && not (IntSet.disjoint kept dependents))
                        )
                        False
                        (certUniqueDefiners c)
                checkSid sid rest =
                    case IntMap.lookup sid (certMissingDefReqs c) of
                        Nothing -> rest
                        Just reqs -> any (IntSet.disjoint kept) reqs || rest

-- | Byte-exact prefix certificate check against engine-verified prefix slices.
checkEarlyRejectionBytes :: EarlyRejectCertifier -> ByteString -> Maybe EarlyRejectReason
checkEarlyRejectionBytes cert candidateSQL
    | IntSet.null (certExactPrefixByteLens cert) = Nothing
    | let !candLen = BS.length candidateSQL
    , IntSet.member candLen (certExactPrefixByteLens cert)
    , candidateSQL == BS.take candLen (certBaseSQL cert) =
        Just EarlyRejectExactPrefix
    | otherwise = Nothing

{- | Check whether an intra-statement syntax edit is certified rejected because
it deletes one or more projections from the outermost wildcard-free final result
@SELECT@ (strictly changing result column arity).
-}
checkEarlyRejectionSyntaxEdit ::
    [StatementUnit] -> EarlyRejectCertifier -> ParserEdit -> Maybe EarlyRejectReason
checkEarlyRejectionSyntaxEdit currentUnits cert edit
    | not (parserEditTopProjection edit) = Nothing
    | Just (expectedCount, lastIdx) <- certProtectedResultIndex cert
    , length currentUnits == expectedCount
    , all (isNothing . unitParseError) currentUnits
    , resultUnit : trailing <- drop lastIdx currentUnits
    , all unitCannotProduceResult trailing
    , let Span resStart resEnd = stmtSpan (unitStatement resultUnit)
    , parserEditStart edit >= resStart && parserEditEnd edit <= resEnd =
        Just EarlyRejectColumnArityMismatch
    | otherwise = Nothing

-- | Combined certificate check over both retained statement IDs and rendered bytes.
checkEarlyRejection :: EarlyRejectCertifier -> IntSet -> ByteString -> Maybe EarlyRejectReason
checkEarlyRejection cert keptInts candidateSQL =
    case checkEarlyRejectionIds cert keptInts of
        hit@(Just _) -> hit
        Nothing -> checkEarlyRejectionBytes cert candidateSQL

checkEarlyRejectionIdsTracked ::
    EarlyRejectTracker -> EarlyRejectCertifier -> IntSet -> IO (Maybe EarlyRejectReason)
checkEarlyRejectionIdsTracked tracker cert keptInts
    | not (certEnabled cert) = pure Nothing
    | otherwise = do
        t0 <- getMonotonicTimeNSec
        let !res = checkEarlyRejectionIds cert keptInts
        t1 <- getMonotonicTimeNSec
        recordCheck (trackerStatsRef tracker) True res (t1 - t0)
        pure res

checkEarlyRejectionBytesTracked ::
    EarlyRejectTracker -> EarlyRejectCertifier -> Bool -> ByteString -> IO (Maybe EarlyRejectReason)
checkEarlyRejectionBytesTracked tracker cert countChecked candidateSQL
    | IntSet.null (certExactPrefixByteLens cert) && isNothing (trackerFailures tracker) =
        pure Nothing
    | otherwise = do
        t0 <- getMonotonicTimeNSec
        res <- case checkEarlyRejectionBytes cert candidateSQL of
            hit@(Just _) -> pure hit
            Nothing -> checkFailingPrefixReason (trackerFailures tracker) candidateSQL
        t1 <- getMonotonicTimeNSec
        recordCheck (trackerStatsRef tracker) countChecked res (t1 - t0)
        pure res

checkFailingPrefixReason :: Maybe FailingPrefixCache -> ByteString -> IO (Maybe EarlyRejectReason)
checkFailingPrefixReason Nothing _ = pure Nothing
checkFailingPrefixReason (Just fpCache) candidateSQL = do
    failed <- checkFailingPrefix fpCache candidateSQL
    pure (if failed then Just EarlyRejectFailingPrefix else Nothing)

checkEarlyRejectionSyntaxEditTracked ::
    EarlyRejectTracker ->
    [StatementUnit] ->
    EarlyRejectCertifier ->
    ParserEdit ->
    IO (Maybe EarlyRejectReason)
checkEarlyRejectionSyntaxEditTracked tracker currentUnits cert edit
    | isNothing (certProtectedResultIndex cert) || not (parserEditTopProjection edit) =
        pure Nothing
    | otherwise = do
        t0 <- getMonotonicTimeNSec
        let !res = checkEarlyRejectionSyntaxEdit currentUnits cert edit
        t1 <- getMonotonicTimeNSec
        recordCheck (trackerStatsRef tracker) True res (t1 - t0)
        pure res

recordCheck :: IORef EarlyRejectStats -> Bool -> Maybe EarlyRejectReason -> Word64 -> IO ()
recordCheck ref countChecked reason !dt =
    modifyIORef' ref $ \stats ->
        stats
            { earlyRejectChecked = earlyRejectChecked stats + if countChecked then 1 else 0
            , earlyRejectExactPrefix =
                earlyRejectExactPrefix stats + if reason == Just EarlyRejectExactPrefix then 1 else 0
            , earlyRejectNoResult =
                earlyRejectNoResult stats + if reason == Just EarlyRejectNoResultStatement then 1 else 0
            , earlyRejectFailingPrefix =
                earlyRejectFailingPrefix stats + if reason == Just EarlyRejectFailingPrefix then 1 else 0
            , earlyRejectMissingDef =
                earlyRejectMissingDef stats + if reason == Just EarlyRejectMissingDefinition then 1 else 0
            , earlyRejectColumnArity =
                earlyRejectColumnArity stats + if reason == Just EarlyRejectColumnArityMismatch then 1 else 0
            , earlyRejectCheckNanos = earlyRejectCheckNanos stats + dt
            }
