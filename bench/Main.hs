module Main (main) where

import Control.Monad (forM_, unless)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (..), getRTSStats)
import Occam.Core (SemanticFacts (..), Span (..), Statement (..), StatementId (..))
import Occam.Dependency (DependencyGraph (..), analyzeStatements)
import Occam.Facts (StatementFacts (..), StatementUnit (..))
import Occam.Metrics (ParseKind (..))
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    defaultParseOptions,
    parseSQLTimed,
 )
import Occam.Reducer (closeSet, cutRemovedStatementIds, replaceSpan)
import Occam.Slicer (backwardSlice)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Printf (printf)

{- | One measured row: total wall time plus exact RTS-allocated bytes over
'benchReps' iterations, with the live-bytes high-water mark observed after.
-}
data BenchResult = BenchResult
    { benchName :: !String
    , benchStatements :: !Int
    , benchReps :: !Int
    , benchWallMicros :: !Integer
    , benchAllocBytes :: !Word64
    , benchPeakLiveBytes :: !Word64
    }
    deriving (Eq, Show)

benchResultJson :: BenchResult -> Value
benchResultJson BenchResult {..} =
    object
        [ "name" .= benchName
        , "statements" .= benchStatements
        , "reps" .= benchReps
        , "wall_micros" .= benchWallMicros
        , "alloc_bytes" .= benchAllocBytes
        , "peak_live_bytes" .= benchPeakLiveBytes
        ]

data BenchOptions = BenchOptions
    { benchQuick :: !Bool
    , benchRepsOverride :: !(Maybe Int)
    , benchJsonPath :: !(Maybe FilePath)
    }
    deriving (Eq, Show)

defaultBenchOptions :: BenchOptions
defaultBenchOptions = BenchOptions True Nothing Nothing

parseBenchArgs :: [String] -> IO BenchOptions
parseBenchArgs = go defaultBenchOptions
    where
        go opts [] = pure opts
        go opts ("--quick" : rest) = go opts {benchQuick = True} rest
        go opts ("--full" : rest) = go opts {benchQuick = False} rest
        go opts ("--json" : path : rest) = go opts {benchJsonPath = Just path} rest
        go opts ("--reps" : count : rest) = case reads count of
            [(n, "")] | n > (0 :: Int) -> go opts {benchRepsOverride = Just n} rest
            _ -> die ("occam-bench: expected a positive --reps count, got: " ++ count)
        go _ (flag : _) = die ("occam-bench: unknown argument: " ++ flag ++ usage)
        usage = "\nUsage: occam-bench [--quick|--full] [--reps N] [--json FILE]"

repsFor :: BenchOptions -> Int -> Int
repsFor opts stmts = case benchRepsOverride opts of
    Just n -> n
    Nothing
        | benchQuick opts -> if stmts <= 64 then 10 else 3
        | otherwise -> if stmts <= 64 then 200 else 50

{- | Microsecond-scale slicer/surgery loops need more repetitions than the
millisecond-scale parse/analyze stages to clear RTS counter granularity.
-}
repsMany :: BenchOptions -> Int
repsMany opts = case benchRepsOverride opts of
    Just n -> n
    Nothing
        | benchQuick opts -> 300
        | otherwise -> 3000

-- | Eight dependency-linked statements per table: DDL, writes, and reads.
tableBlock :: Int -> [ByteString]
tableBlock i =
    map
        BSC.pack
        [ "CREATE TABLE t" ++ s ++ " (a INT PRIMARY KEY, b TEXT);"
        , "INSERT INTO t" ++ s ++ " (a, b) VALUES (1, 'x');"
        , "INSERT INTO t" ++ s ++ " (a, b) VALUES (2, 'y');"
        , "SELECT a, b FROM t" ++ s ++ " WHERE a > 1 ORDER BY a;"
        , "UPDATE t" ++ s ++ " SET b = 'z' WHERE a = 1;"
        , "DELETE FROM t" ++ s ++ " WHERE a = 2;"
        , "SELECT COUNT(*) FROM t" ++ s ++ ";"
        , "INSERT INTO t" ++ s ++ " (a, b) VALUES (3, 'w');"
        ]
    where
        s = show i

syntheticSQL :: Int -> ByteString
syntheticSQL tables = BSC.unlines (concatMap tableBlock [0 .. tables - 1])

-- | Parser payload per tier, mirroring the reducer's own option sets.
optionsForKind :: ParseKind -> ParseOptions
optionsForKind ParseStatementOnly = defaultParseOptions GenericSQL
optionsForKind ParseSyntaxChunks =
    (defaultParseOptions GenericSQL) {parseChunkEdits = True, parseSyntaxEdits = True}
optionsForKind ParseColumnIR =
    (defaultParseOptions GenericSQL)
        { parseChunkEdits = True
        , parseSyntaxEdits = True
        , parseColumnIR = True
        , parseColumnBundles = True
        }

kindLabel :: ParseKind -> String
kindLabel ParseStatementOnly = "stmt-only"
kindLabel ParseSyntaxChunks = "syntax-chunks"
kindLabel ParseColumnIR = "column-ir"

parseUnitsOrDie :: ParseKind -> ByteString -> IO ParsedSQL
parseUnitsOrDie kind sql = do
    parsed <- parseSQLTimed Nothing kind (optionsForKind kind) sql
    case parsed of
        Left err -> die ("occam-bench: synthetic SQL failed to parse: " ++ show err)
        Right ok -> pure ok

-- | Spine-plus-payload checksum so laziness cannot hide decode work.
parsedWorkload :: ParsedSQL -> Int
parsedWorkload parsed = sum (map unitWork (parsedUnits parsed))
    where
        unitWork unit =
            let Span start end = stmtSpan (unitStatement unit)
             in (end - start) + case unitFacts unit of
                    Nothing -> 0
                    Just facts -> length (factOps (factSemantics facts)) + length (factEdits facts)

graphWorkload :: DependencyGraph -> Int
graphWorkload graph = Map.foldl' (\total deps -> total + Set.size deps) 0 (graphEdges graph)

{- | Strict byte-sum checksum. Length alone lets GHC eliminate the very copy
under test for statically shaped splices, so surgery rows fold every byte.
-}
byteChecksum :: ByteString -> Int
byteChecksum = BS.foldl' (\total w -> total + fromIntegral w) 0

-- | Eight-id window rotating with the iteration so every repetition re-slices.
windowIds :: [StatementId] -> Int -> Set.Set StatementId
windowIds ids i = Set.fromList (take 8 (drop ((i * 8) `mod` (length ids - 8)) ids))

{- | Rebuild the unit spine per iteration so GHC cannot share one analysis
across repetitions. The statements are identical; only the spine is fresh.
-}
freshUnits :: Int -> [StatementUnit] -> [StatementUnit]
freshUnits i units = take (length units - (i `mod` 1)) units

checkCount :: Int -> ParsedSQL -> IO ()
checkCount expected parsed =
    let actual = length (parsedUnits parsed)
     in unless (actual == expected) $
            die ("occam-bench: expected " ++ show expected ++ " statements, parsed " ++ show actual)

{- | The iteration index reaches every action so loop-invariant pure work
cannot be shared across repetitions and silently measured once.
-}
timeReps :: Int -> (Int -> IO Int) -> IO (Integer, Word64, Word64)
timeReps reps action = do
    beforeAlloc <- allocated_bytes <$> getRTSStats
    before <- getMonotonicTimeNSec
    _ <- loop 0 reps 0
    after <- getMonotonicTimeNSec
    stats <- getRTSStats
    pure
        ( fromIntegral (after - before) `div` 1000
        , allocated_bytes stats - beforeAlloc
        , max_live_bytes stats
        )
    where
        loop _ 0 !accumulated = pure accumulated
        loop index remaining !accumulated = do
            work <- action index
            loop (index + 1) (remaining - 1) (accumulated + work)

runBench :: String -> Int -> Int -> (Int -> IO Int) -> IO BenchResult
runBench name stmts reps action = do
    (wall, alloc, peak) <- timeReps reps action
    pure (BenchResult name stmts reps wall alloc peak)

printTable :: [BenchResult] -> IO ()
printTable rows = do
    putStrLn "occam-bench: Tier 1 in-process engine benchmark (no external processes)"
    printf
        ("%-24s %5s %5s %12s %10s %12s %10s %12s\n" :: String)
        ("benchmark" :: String)
        ("stmts" :: String)
        ("reps" :: String)
        ("wall_us" :: String)
        ("us_iter" :: String)
        ("alloc_B" :: String)
        ("B_iter" :: String)
        ("peak_live_B" :: String)
    forM_ rows $ \BenchResult {..} -> do
        let usPerIter = benchWallMicros `div` fromIntegral benchReps
            bytesPerIter = benchAllocBytes `div` fromIntegral benchReps
        printf
            ("%-24s %5d %5d %12d %10d %12d %10d %12d\n" :: String)
            benchName
            benchStatements
            benchReps
            benchWallMicros
            usPerIter
            benchAllocBytes
            bytesPerIter
            benchPeakLiveBytes

main :: IO ()
main = do
    opts <- parseBenchArgs =<< getArgs
    let sql64 = syntheticSQL 8
        sql512 = syntheticSQL 64
    -- Pre-parse once for the DAG/surgery stages; parsing itself is timed below.
    parsed64 <- parseUnitsOrDie ParseStatementOnly sql64
    parsed512 <- parseUnitsOrDie ParseStatementOnly sql512
    checkCount 64 parsed64
    checkCount 512 parsed512
    let units64 = parsedUnits parsed64
        units512 = parsedUnits parsed512
        sess64 = parsedInitialSession parsed64
        sess512 = parsedInitialSession parsed512
        graph64 = analyzeStatements sess64 units64
        graph512 = analyzeStatements sess512 units512
        ids64 = map (stmtId . unitStatement) units64
        ids512 = map (stmtId . unitStatement) units512
        targets64 = drop (length ids64 - 8) ids64
        targets512 = drop (length ids512 - 8) ids512
        stmts512 = map unitStatement units512
        keptEven = Set.fromList [stmtId stmt | (i, stmt) <- zip [0 :: Int ..] stmts512, even i]
        keptOdd = Set.fromList [stmtId stmt | (i, stmt) <- zip [0 :: Int ..] stmts512, odd i]
        smallReps = repsFor opts 64
        bigReps = repsFor opts 512
        manyReps = repsMany opts
    parseRows <-
        sequence
            [ runBench ("parse/" ++ kindLabel kind) stmts reps (\_ -> parsedWorkload <$> parseUnitsOrDie kind sql)
            | (sql, stmts, reps) <- [(sql64, 64, smallReps), (sql512, 512, bigReps)]
            , kind <- [ParseStatementOnly, ParseSyntaxChunks, ParseColumnIR]
            ]
    dagRows <-
        sequence
            [ runBench
                "dag/analyze"
                64
                smallReps
                (\i -> pure (graphWorkload (analyzeStatements sess64 (freshUnits i units64))))
            , runBench
                "dag/analyze"
                512
                bigReps
                (\i -> pure (graphWorkload (analyzeStatements sess512 (freshUnits i units512))))
            , runBench
                "dag/backward-slice"
                64
                manyReps
                (\i -> pure (Set.size (backwardSlice (targets64 !! (i `mod` 8)) graph64)))
            , runBench
                "dag/backward-slice"
                512
                manyReps
                (\i -> pure (Set.size (backwardSlice (targets512 !! (i `mod` 8)) graph512)))
            , runBench "dag/close-set" 64 manyReps (pure . Set.size . closeSet graph64 . windowIds ids64)
            , runBench
                "dag/close-set"
                512
                manyReps
                (pure . Set.size . closeSet graph512 . windowIds ids512)
            ]
    surgeryRows <-
        sequence
            [ runBench
                "surgery/cut-removed"
                512
                manyReps
                ( \i ->
                    pure (byteChecksum (cutRemovedStatementIds sql512 stmts512 (if even i then keptEven else keptOdd)))
                )
            , runBench
                "surgery/replace-span"
                512
                manyReps
                ( \i ->
                    let start = 128 + (i `mod` 16) * 64
                     in pure (byteChecksum (replaceSpan start (start + 896) ("/* trimmed */" :: ByteString) sql512))
                )
            ]
    let rows = parseRows ++ dagRows ++ surgeryRows
    printTable rows
    case benchJsonPath opts of
        Nothing -> pure ()
        Just path -> BSL.writeFile path (encode (map benchResultJson rows))
