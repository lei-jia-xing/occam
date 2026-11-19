module Main (main) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Occam.Cache (
    CacheMode (..),
    cacheCompleteInflight,
    cacheLookup,
    newCache,
 )
import Occam.Core (SemanticFacts (..), Span (..), Statement (..), StatementId (..))
import Occam.Dependency (DependencyGraph (..), analyzeStatements)
import Occam.Facts (StatementFacts (..), StatementUnit (..))
import Occam.Metrics (ParseKind (..))
import Occam.Oracle (OracleObservation (..))
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    defaultParseOptions,
    parseSQLTimed,
 )
import Occam.Reducer (closeSet, cutRemovedStatementIds, replaceSpan)
import Occam.Slicer (backwardSlice)
import System.Exit (ExitCode (..), die)
import Test.Tasty.Bench (
    bcompare,
    bench,
    bgroup,
    defaultMain,
    whnf,
    whnfIO,
 )

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

checkCount :: Int -> ParsedSQL -> IO ()
checkCount expected parsed =
    let actual = length (parsedUnits parsed)
     in unless (actual == expected) $
            die ("occam-bench: expected " ++ show expected ++ " statements, parsed " ++ show actual)

main :: IO ()
main = do
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
    cache <- newCache Map.empty
    let candidateKey = BSC.pack "SELECT * FROM t0 WHERE a = 1;"
        nonExistentKey = BSC.pack "SELECT * FROM non_existent WHERE x = 999;"
        obs = OracleFinished ExitSuccess
    cacheCompleteInflight cache candidateKey 0 (Just obs)
    defaultMain
        [ bgroup
            "parse"
            [ bgroup
                "64"
                [ bench "stmt-only" $ whnfIO (parsedWorkload <$> parseUnitsOrDie ParseStatementOnly sql64)
                , bcompare "$NF == \"stmt-only\" && $(NF-1) == \"64\"" $
                    bench "syntax-chunks" $
                        whnfIO (parsedWorkload <$> parseUnitsOrDie ParseSyntaxChunks sql64)
                , bcompare "$NF == \"stmt-only\" && $(NF-1) == \"64\"" $
                    bench "column-ir" $
                        whnfIO (parsedWorkload <$> parseUnitsOrDie ParseColumnIR sql64)
                ]
            , bgroup
                "512"
                [ bench "stmt-only" $ whnfIO (parsedWorkload <$> parseUnitsOrDie ParseStatementOnly sql512)
                , bcompare "$NF == \"stmt-only\" && $(NF-1) == \"512\"" $
                    bench "syntax-chunks" $
                        whnfIO (parsedWorkload <$> parseUnitsOrDie ParseSyntaxChunks sql512)
                , bcompare "$NF == \"stmt-only\" && $(NF-1) == \"512\"" $
                    bench "column-ir" $
                        whnfIO (parsedWorkload <$> parseUnitsOrDie ParseColumnIR sql512)
                ]
            ]
        , bgroup
            "dag"
            [ bgroup
                "analyze"
                [ bench "64" $ whnf (graphWorkload . analyzeStatements sess64) units64
                , bcompare "$NF == \"64\" && $(NF-1) == \"analyze\"" $
                    bench "512" $
                        whnf (graphWorkload . analyzeStatements sess512) units512
                ]
            , bgroup
                "backward-slice"
                [ bench "64" $ whnf (Set.size . flip backwardSlice graph64) (last targets64)
                , bcompare "$NF == \"64\" && $(NF-1) == \"backward-slice\"" $
                    bench "512" $
                        whnf (Set.size . flip backwardSlice graph512) (last targets512)
                ]
            , bgroup
                "close-set"
                [ bench "64" $ whnf (Set.size . closeSet graph64) (windowIds ids64 0)
                , bcompare "$NF == \"64\" && $(NF-1) == \"close-set\"" $
                    bench "512" $
                        whnf (Set.size . closeSet graph512) (windowIds ids512 0)
                ]
            ]
        , bgroup
            "surgery"
            [ bench "cut-removed" $ whnf (byteChecksum . cutRemovedStatementIds sql512 stmts512) keptEven
            , bcompare "cut-removed" $
                bench "replace-span" $
                    whnf
                        (\start -> byteChecksum (replaceSpan start (start + 896) ("/* trimmed */" :: ByteString) sql512))
                        (128 :: Int)
            ]
        , bgroup
            "cache"
            [ bench "hit" $ whnfIO (cacheLookup cache CacheEnabled Map.empty candidateKey)
            , bcompare "hit" $ bench "miss" $ whnfIO (cacheLookup cache CacheEnabled Map.empty nonExistentKey)
            ]
        ]
