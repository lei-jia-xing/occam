-- | Provably sound early-rejection certificates for native result oracles.
module Occam.Certificate (
    EarlyRejectCertifier (..),
    EarlyRejectStats (..),
    EarlyRejectTracker,
    emptyEarlyRejectCertifier,
    emptyEarlyRejectStats,
    buildEarlyRejectCertifier,
    checkEarlyRejection,
    checkEarlyRejectionIds,
    checkEarlyRejectionBytes,
    newEarlyRejectTracker,
    readEarlyRejectStats,
    recordEarlyRejectBuildNanos,
    checkEarlyRejectionIdsTracked,
    checkEarlyRejectionBytesTracked,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (isNothing)
import Data.Word (Word64, Word8)
import GHC.Clock (getMonotonicTimeNSec)
import Occam.Core (
    EarlyRejectReason (..),
    ResultTrace (..),
    Span (..),
    Statement (..),
    StatementId (..),
 )
import Occam.Facts (StatementUnit (..))

-- | Precomputed early-rejection certificates anchored to one accepted base SQL script.
data EarlyRejectCertifier = EarlyRejectCertifier
    { certEnabled :: !Bool
    , certContiguousIds :: !Bool
    , certBaseSQL :: !ByteString
    , certExactPrefixByteLens :: !IntSet
    , certExactPrefixLengths :: !IntSet
    , certNonResultReady :: !Bool
    , certGuaranteedNonResult :: !IntSet
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
        }

-- | Per-stage certificate check and hit accounting, including checking overhead.
data EarlyRejectStats = EarlyRejectStats
    { earlyRejectChecked :: !Int
    , earlyRejectExactPrefix :: !Int
    , earlyRejectNoResult :: !Int
    , earlyRejectCheckNanos :: !Word64
    }
    deriving (Eq, Show)

emptyEarlyRejectStats :: EarlyRejectStats
emptyEarlyRejectStats = EarlyRejectStats 0 0 0 0

newtype EarlyRejectTracker = EarlyRejectTracker (IORef EarlyRejectStats)

newEarlyRejectTracker :: IO EarlyRejectTracker
newEarlyRejectTracker = EarlyRejectTracker <$> newIORef emptyEarlyRejectStats

readEarlyRejectStats :: EarlyRejectTracker -> IO EarlyRejectStats
readEarlyRejectStats (EarlyRejectTracker ref) = readIORef ref

recordEarlyRejectBuildNanos :: EarlyRejectTracker -> Word64 -> IO ()
recordEarlyRejectBuildNanos (EarlyRejectTracker ref) nanos =
    modifyIORef' ref (\stats -> stats {earlyRejectCheckNanos = earlyRejectCheckNanos stats + nanos})

{- | Build early-rejection certificates from an accepted script, its parsed
statement units, and native engine trace evidence.

Two sound certificate classes are derived:

1. 'EarlyRejectExactPrefix': When the native run was deterministic and recorded
   exact engine statement end byte offsets, any proper prefix @0..k@ whose
   latest result did not match the contract is certified rejected. Trailing
   ASCII-whitespace-only padding up to the next statement start is also
   included so exact byte slices from statement cutting match directly.
   Exact-prefix byte matching stores only valid prefix byte lengths in an
   'IntSet' alongside 'certBaseSQL', giving O(N) build time and O(1) rejection
   for non-matching lengths while remaining byte-for-byte exact.

2. 'EarlyRejectNoResultStatement': When engine and parser statement counts agree
   with zero parse errors and the script produced a required result set, any
   subsequence whose retained statements all carry the strict AST
   @unitCannotProduceResult@ allowlist bit and produced no result in the native
   trace cannot produce a result set and is certified rejected.
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

        (prefixByteLens, prefixLengths) =
            case traceEndByteOffsets trace of
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
                    (!byteLens2, !lens2) = case mNextStart of
                        Just nextStart
                            | countsMatch
                            , endByte <= nextStart
                            , nextStart <= baseLen
                            , isAsciiWhitespace (BS.take (nextStart - endByte) (BS.drop endByte baseSQL)) ->
                                ( IntSet.insert nextStart byteLens1
                                , if contiguousIds then IntSet.insert (k + 1) lensAcc else lensAcc
                                )
                        _ -> (byteLens1, lensAcc)
                 in collectPrefixes rest byteLens2 lens2

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

        enabled = not (IntSet.null prefixByteLens) || nonResultReady

isAsciiWhitespace :: ByteString -> Bool
isAsciiWhitespace = BS.all isWsByte
    where
        isWsByte :: Word8 -> Bool
        isWsByte b = b == 0x20 || b == 0x09 || b == 0x0a || b == 0x0d

-- | Fast O(1) certificate check over retained statement IDs before rendering SQL bytes.
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
    | otherwise = Nothing

-- | Byte-exact prefix certificate check against engine-verified prefix slices.
checkEarlyRejectionBytes :: EarlyRejectCertifier -> ByteString -> Maybe EarlyRejectReason
checkEarlyRejectionBytes cert candidateSQL
    | IntSet.null (certExactPrefixByteLens cert) = Nothing
    | let !candLen = BS.length candidateSQL
    , IntSet.member candLen (certExactPrefixByteLens cert)
    , candidateSQL == BS.take candLen (certBaseSQL cert) =
        Just EarlyRejectExactPrefix
    | otherwise = Nothing

-- | Combined certificate check over both retained statement IDs and rendered bytes.
checkEarlyRejection :: EarlyRejectCertifier -> IntSet -> ByteString -> Maybe EarlyRejectReason
checkEarlyRejection cert keptInts candidateSQL =
    case checkEarlyRejectionIds cert keptInts of
        hit@(Just _) -> hit
        Nothing -> checkEarlyRejectionBytes cert candidateSQL

checkEarlyRejectionIdsTracked ::
    EarlyRejectTracker -> EarlyRejectCertifier -> IntSet -> IO (Maybe EarlyRejectReason)
checkEarlyRejectionIdsTracked (EarlyRejectTracker ref) cert keptInts
    | not (certEnabled cert) = pure Nothing
    | otherwise = do
        t0 <- getMonotonicTimeNSec
        let !res = checkEarlyRejectionIds cert keptInts
        t1 <- getMonotonicTimeNSec
        recordCheck ref True res (t1 - t0)
        pure res

checkEarlyRejectionBytesTracked ::
    EarlyRejectTracker -> EarlyRejectCertifier -> Bool -> ByteString -> IO (Maybe EarlyRejectReason)
checkEarlyRejectionBytesTracked (EarlyRejectTracker ref) cert countChecked candidateSQL
    | IntSet.null (certExactPrefixByteLens cert) = pure Nothing
    | otherwise = do
        t0 <- getMonotonicTimeNSec
        let !res = checkEarlyRejectionBytes cert candidateSQL
        t1 <- getMonotonicTimeNSec
        recordCheck ref countChecked res (t1 - t0)
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
            , earlyRejectCheckNanos = earlyRejectCheckNanos stats + dt
            }
