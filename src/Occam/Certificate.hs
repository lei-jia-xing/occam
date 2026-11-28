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
    prepareDeletionCertificateCheck,
    checkEarlyRejectionBytesTracked,
    checkEarlyRejectionSyntaxEditTracked,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable qualified as Foldable
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (isJust, isNothing)
import Data.Word (Word64, Word8)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Certificate.MissingDef (buildMissingDefReqs)
import Occam.Certificate.PrefixCache (
    FailingPrefixCache,
    checkFailingPrefix,
    newFailingPrefixCache,
    recordFailingPrefix,
 )
import Occam.Core.Types (
    EarlyRejectReason (..),
    ResultTrace (..),
    Span (..),
    Statement (..),
    StatementId (..),
 )
import Occam.Facts (ParserEdit (..), StatementUnit (..))

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

-- | Zeroed 'EarlyRejectStats' counter record.
emptyEarlyRejectStats :: EarlyRejectStats
emptyEarlyRejectStats = EarlyRejectStats 0 0 0 0 0 0 0

-- | Thread-safe accumulator of early-rejection statistics paired with an optional failing-prefix cache.
data EarlyRejectTracker = EarlyRejectTracker
    { trackerStatsRef :: !(IORef EarlyRejectStats)
    , trackerFailures :: !(Maybe FailingPrefixCache)
    }

-- | Create a standalone 'EarlyRejectTracker' with no failing-prefix cache.
newEarlyRejectTracker :: IO EarlyRejectTracker
newEarlyRejectTracker = newEarlyRejectTrackerWithFailures Nothing

-- | Create an 'EarlyRejectTracker' sharing an optional dynamic failing-prefix cache.
newEarlyRejectTrackerWithFailures :: Maybe FailingPrefixCache -> IO EarlyRejectTracker
newEarlyRejectTrackerWithFailures mFailures =
    EarlyRejectTracker <$> newIORef emptyEarlyRejectStats <*> pure mFailures

-- | Snapshot cumulative early-rejection counts and timing from the tracker.
readEarlyRejectStats :: EarlyRejectTracker -> IO EarlyRejectStats
readEarlyRejectStats = readIORef . trackerStatsRef

-- | Add certificate-construction nanoseconds into the tracker telemetry total.
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
   padding up to the next statement start is also included when it contains
   only whitespace or, after a terminator, ordinary comments and empty
   statements.

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
        addUniqueDefiners acc sid = Foldable.foldl' (addUniqueDefiner sid) acc
        addUniqueDefiner sid m req
            | Just defSid <- uniqueDefiner sid req =
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
                    (!byteLens2, !lens2) = extendWithPadding k endByte mNextStart byteLens1 lensAcc
                 in collectPrefixes rest byteLens2 lens2

        extendWithPadding k endByte (Just nextStart) !byteLens1 !lensAcc
            | countsMatch
            , endByte <= nextStart
            , nextStart <= baseLen
            , let padding = BS.take (nextStart - endByte) (BS.drop endByte baseSQL)
            , BS.all isAsciiWhitespace padding
                || (endByte > 0 && BS.index baseSQL (endByte - 1) == 59 && isInertPadding padding) =
                ( IntSet.insert nextStart byteLens1
                , if contiguousIds then IntSet.insert (k + 1) lensAcc else lensAcc
                )
        extendWithPadding _ _ _ !byteLens1 !lensAcc = (byteLens1, lensAcc)

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

isAsciiWhitespace :: Word8 -> Bool
isAsciiWhitespace b = b == 0x20 || b == 0x09 || b == 0x0a || b == 0x0d

-- The native result dialects share these ordinary comments and separators.
-- Accept LF and CRLF for line comments. A bare CR can be part of a SQLite
-- comment. Use only complete, nonnested block comments.
isInertPadding :: ByteString -> Bool
isInertPadding = scan . BS.dropWhile isAsciiWhitespace
    where
        scan bytes
            | BS.null bytes = True
            | BS.head bytes == 59 = isInertPadding (BS.tail bytes)
            | Just line <- BS.stripPrefix "--" bytes =
                case BS.uncons (BS.dropWhile (\b -> b /= 10 && b /= 13) line) of
                    Nothing -> True
                    Just (10, rest) -> isInertPadding rest
                    Just (13, rest) -> maybe False isInertPadding (BS.stripPrefix "\n" rest)
                    _ -> False
            | Just block <- BS.stripPrefix "/*" bytes
            , not (BS.null block)
            , BS.head block /= 33
            , BS.head block /= 43
            , let (body, rest) = BS.breakSubstring "*/" block
            , not (BS.null rest)
            , not ("/*" `BS.isInfixOf` body) =
                isInertPadding (BS.drop 2 rest)
            | otherwise = False

uniqueDefiner :: Int -> IntSet -> Maybe Int
uniqueDefiner sid req
    | not (IntSet.null req)
    , let !definer = IntSet.findMin req
    , definer /= sid
    , IntSet.findMax req == definer =
        Just definer
    | otherwise = Nothing

-- | Fast certificate check over retained statement IDs before rendering SQL bytes.
checkEarlyRejectionIds :: EarlyRejectCertifier -> IntSet -> Maybe EarlyRejectReason
checkEarlyRejectionIds cert kept =
    checkEarlyRejectionIdsWith
        cert
        kept
        (kept `IntSet.isSubsetOf` certGuaranteedNonResult cert)
        (hasMissingDefinition cert kept)

checkEarlyRejectionIdsWith ::
    EarlyRejectCertifier -> IntSet -> Bool -> Bool -> Maybe EarlyRejectReason
checkEarlyRejectionIdsWith cert kept noResult missingDefinition
    | not (certEnabled cert) = Nothing
    | certContiguousIds cert
    , not (IntSet.null (certExactPrefixLengths cert))
    , not (IntSet.null kept)
    , let !prefixLen = IntSet.findMax kept
    , IntSet.member prefixLen (certExactPrefixLengths cert)
    , IntSet.findMin kept == 1
    , IntSet.size kept == prefixLen =
        Just EarlyRejectExactPrefix
    | certNonResultReady cert && noResult =
        Just EarlyRejectNoResultStatement
    | missingDefinition =
        Just EarlyRejectMissingDefinition
    | otherwise = Nothing

hasMissingDefinition :: EarlyRejectCertifier -> IntSet -> Bool
hasMissingDefinition cert kept
    | IntMap.null requirements = False
    | otherwise = anyMissingUniqueDef || IntSet.foldr checkSid False kept
    where
        requirements = certMissingDefReqs cert
        anyMissingUniqueDef =
            IntMap.foldrWithKey
                ( \defSid dependents rest ->
                    (IntSet.notMember defSid kept && not (IntSet.disjoint kept dependents)) || rest
                )
                False
                (certUniqueDefiners cert)
        checkSid sid rest =
            case IntMap.lookup sid requirements of
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

-- | Check a statement-ID subset against the certifier while recording hit\/miss telemetry.
checkEarlyRejectionIdsTracked ::
    EarlyRejectTracker -> EarlyRejectCertifier -> IntSet -> IO (Maybe EarlyRejectReason)
checkEarlyRejectionIdsTracked tracker cert kept =
    trackIdCheck tracker cert (checkEarlyRejectionIds cert kept)

{- | Prepare the ID check for an audit that starts from the supplied set and
only commits single deletions that pass the check. The arguments to the
returned check are the removed ID and the retained set after the removal.
-}
prepareDeletionCertificateCheck ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    IntSet ->
    IO (Int -> IntSet -> IO (Maybe EarlyRejectReason))
prepareDeletionCertificateCheck tracker cert initialKept
    | not (certEnabled cert) = pure (\_ _ -> pure Nothing)
    | otherwise = do
        t0 <- getMonotonicTimeNSec
        let !check = preparedCheck
        t1 <- getMonotonicTimeNSec
        recordEarlyRejectBuildNanos tracker (t1 - t0)
        pure check
    where
        preparedCheck
            | hasMissingDefinition cert initialKept = \_ -> checkEarlyRejectionIdsTracked tracker cert
            | otherwise =
                let !possibleResults = initialKept `IntSet.difference` certGuaranteedNonResult cert
                    !alternatives = IntMap.foldlWithKey' addAlternatives IntMap.empty (certMissingDefReqs cert)
                 in \removed kept ->
                        trackIdCheck tracker cert $
                            checkEarlyRejectionIdsWith
                                cert
                                kept
                                (IntSet.disjoint kept possibleResults)
                                (missingUniqueDef removed kept || missingAlternatives alternatives kept)

        -- A satisfied singleton requirement can only become invalid when its
        -- sole definer is removed and at least one dependent survives.
        missingUniqueDef removed kept =
            case IntMap.lookup removed (certUniqueDefiners cert) of
                Nothing -> False
                Just dependents -> not (IntSet.disjoint kept dependents)

        addAlternatives byWitness sid = Foldable.foldl' addRequirement byWitness
            where
                addRequirement index req
                    | IntSet.null req || isJust (uniqueDefiner sid req) = index
                    | otherwise = IntMap.insertWith (++) (IntSet.findMax req) [(sid, req)] index

        -- A retained witness satisfies every requirement in its group.
        -- When it is absent, check the full set of alternative definers.
        missingAlternatives alternatives kept =
            IntMap.foldr
                (\reqs rest -> any missingRequirement reqs || rest)
                False
                (IntMap.withoutKeys alternatives kept)
            where
                missingRequirement (sid, req) = IntSet.member sid kept && IntSet.disjoint kept req

trackIdCheck ::
    EarlyRejectTracker ->
    EarlyRejectCertifier ->
    Maybe EarlyRejectReason ->
    IO (Maybe EarlyRejectReason)
trackIdCheck tracker cert result
    | not (certEnabled cert) = pure Nothing
    | otherwise = do
        t0 <- getMonotonicTimeNSec
        let !res = result
        t1 <- getMonotonicTimeNSec
        recordCheck (trackerStatsRef tracker) True res (t1 - t0)
        pure res

-- | Check rendered SQL bytes against exact-prefix and dynamic failing-prefix caches, updating telemetry.
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

-- | Check a top-level syntax edit against the protected result index and record telemetry.
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
