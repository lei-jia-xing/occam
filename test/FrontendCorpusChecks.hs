-- | Pure-Haskell corpus regression suite across real fuzz, benchmark, and regression cases.
module FrontendCorpusChecks (tests) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, when)
import Data.Aeson (Value (..), eitherDecodeStrict')
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.Maybe (isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Core (
    BarrierReason (..),
    BarrierScope (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    Span (..),
    Statement (..),
 )
import Occam.Facts (
    ColumnSite (..),
    ColumnTarget (..),
    EditTier (..),
    ParserEdit (..),
    ParserEditCategory (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Frontend.Span (isUtf8Boundary)
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    defaultParseOptions,
    lookupDialectByName,
    parseSQL,
 )
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropExtension, takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tests :: TestTree
tests =
    testGroup
        "Pure-Haskell frontend corpus regression"
        [ testCase "71 real_fuzz_corpus cases parse with zero syntax errors and valid spans/barriers" $ do
            cases <- either assertFailure pure =<< loadRealFuzzCorpus "benchmarks/real_fuzz_corpus"
            assertEqual "real fuzz corpus count" 71 (length cases)
            barrierCounts <- forM cases \item -> do
                parsed <- parseOrFail (corpusId item) (fullOptions (corpusDialect item)) (corpusSql item)
                let parseErrors =
                        [ (idx, err)
                        | (idx, u) <- zip [1 :: Int ..] (parsedUnits parsed)
                        , Just err <- [unitParseError u]
                        ]
                assertEqual
                    ("zero syntax errors for " <> T.unpack (corpusId item))
                    []
                    parseErrors
                assertValidParsed (T.unpack (corpusId item)) (corpusSql item) parsed
                let barriers =
                        [ b
                        | u <- parsedUnits parsed
                        , Just f <- [unitFacts u]
                        , b <- factBarriers (factSemantics f)
                        ]
                    syntaxBarriers = filter ((== SyntaxErrorBarrier) . barrierReason) barriers
                assertEqual
                    ("zero SyntaxErrorBarrier for " <> T.unpack (corpusId item))
                    []
                    syntaxBarriers
                pure (any ((== GlobalScope) . barrierScope) barriers, not (null barriers))
            let withGlobal = length (filter fst barrierCounts)
                zeroGlobal = length barrierCounts - withGlobal
                barrierFree = length (filter (not . snd) barrierCounts)
            assertBool "at least 27 real fuzz cases have zero global fences" (zeroGlobal >= 27)
            assertBool "at least 20 real fuzz cases are completely barrier-free" (barrierFree >= 20)
        , testCase "benchmark corpus parses cleanly under full options with valid spans and typed tiers" $ do
            cases <- either assertFailure pure =<< loadBenchmarkCorpus "benchmarks/corpus"
            assertBool "loaded benchmark corpus" (not (null cases))
            forM_ cases \item -> do
                parsed <- parseOrFail (corpusId item) (fullOptions (corpusDialect item)) (corpusSql item)
                let parseErrors =
                        [ (idx, err)
                        | (idx, u) <- zip [1 :: Int ..] (parsedUnits parsed)
                        , Just err <- [unitParseError u]
                        ]
                assertEqual
                    ("zero syntax errors for " <> T.unpack (corpusId item))
                    []
                    parseErrors
                assertValidParsed (T.unpack (corpusId item)) (corpusSql item) parsed
        , testCase "regression cases file parses without ParserError crashes and preserves invariants" $ do
            cases <- either assertFailure pure =<< loadRegressionCases "test/seeds/regression_cases.sql"
            assertEqual "regression corpus count" 528 (length cases)
            forM_ cases \item -> do
                parsed <- parseOrFail (corpusId item) (fullOptions (corpusDialect item)) (corpusSql item)
                assertValidParsed (T.unpack (corpusId item)) (corpusSql item) parsed
        ]

data CorpusCase = CorpusCase
    { corpusId :: !Text
    , corpusDialect :: !SQLDialect
    , corpusSql :: !ByteString
    }

fullOptions :: SQLDialect -> ParseOptions
fullOptions dialect =
    (defaultParseOptions dialect)
        { parseChunkEdits = True
        , parseColumnIR = True
        , parseColumnBundles = True
        , parseSyntaxEdits = True
        }

parseOrFail :: Text -> ParseOptions -> ByteString -> IO ParsedSQL
parseOrFail cid opts sql = do
    res <- parseSQL opts sql
    case res of
        Left err -> assertFailure (T.unpack cid <> ": unexpected ParserError: " <> show err)
        Right parsed -> pure parsed

assertValidParsed :: String -> ByteString -> ParsedSQL -> IO ()
assertValidParsed label sql parsed = do
    let units = parsedUnits parsed
    checkUnits 0 units
    let unitBounds = IntMap.fromList (zip [1 ..] (map (spanBounds . stmtSpan . unitStatement) units))
    forM_ (parsedColumns parsed) \target ->
        forM_ (targetSites target) \site ->
            case IntMap.lookup (siteStmt site) unitBounds of
                Nothing ->
                    assertFailure (label <> ": column site references missing unit " <> show (siteStmt site))
                Just (uStart, uEnd) -> do
                    let s = siteStart site
                        e = siteEnd site
                    assertBool
                        (label <> ": column site out of bounds: " <> show site)
                        (s >= uStart && s < e && e <= uEnd && isUtf8Boundary sql s && isUtf8Boundary sql e)
    where
        !sqlLen = BS.length sql

        spanBounds (Span s e) = (s, e)

        checkUnits _ [] = pure ()
        checkUnits prevEnd (unit : rest) = do
            let stmt = unitStatement unit
                Span s e = stmtSpan stmt
            assertBool
                (label <> ": invalid statement span " <> show (s, e) <> " after " <> show prevEnd)
                (s >= prevEnd && s < e && e <= sqlLen && isUtf8Boundary sql s && isUtf8Boundary sql e)
            assertEqual
                (label <> ": stmtRaw slice mismatch")
                (BS.take (e - s) (BS.drop s sql))
                (stmtRaw stmt)
            whenErrorFree unit (s, e)
            checkUnits e rest

        whenErrorFree unit (uStart, uEnd) =
            case unitFacts unit of
                Nothing -> pure ()
                Just facts -> do
                    assertBool
                        (label <> ": parsed unit with facts must not have parse error")
                        (isNothing (unitParseError unit))
                    forM_ (factEdits facts) \edit -> do
                        let es = parserEditStart edit
                            ee = parserEditEnd edit
                        assertBool
                            (label <> ": invalid edit span " <> show edit)
                            (es >= uStart && es < ee && ee <= uEnd && isUtf8Boundary sql es && isUtf8Boundary sql ee)
                        case (parserEditCategory edit, parserEditTier edit) of
                            (EditClause, TierClause _) -> pure ()
                            (EditClause, bad) ->
                                assertFailure (label <> ": EditClause missing TierClause: " <> show (edit, bad))
                            (EditExpression, TierExpr _) -> pure ()
                            (EditExpression, bad) ->
                                assertFailure (label <> ": EditExpression missing TierExpr: " <> show (edit, bad))
                            (_, TierDefault) -> pure ()
                            (_, bad) ->
                                assertFailure (label <> ": non-clause/expr edit has non-default tier: " <> show (edit, bad))

loadBenchmarkCorpus :: FilePath -> IO (Either String [CorpusCase])
loadBenchmarkCorpus dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure (Left ("missing benchmark corpus directory: " <> dir))
        else do
            files <- sort . filter ((== ".sql") . takeExtension) <$> listDirectory dir
            cases <- forM files \name -> do
                sql <- BS.readFile (dir </> name)
                pure (CorpusCase ("corpus:" <> T.pack name) SQLite sql)
            pure (Right cases)

loadRealFuzzCorpus :: FilePath -> IO (Either String [CorpusCase])
loadRealFuzzCorpus dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then pure (Left ("missing real fuzz corpus directory: " <> dir))
        else do
            files <- sort . filter ((== ".sql") . takeExtension) <$> listDirectory dir
            results <- forM files \name -> do
                let sqlPath = dir </> name
                    metaPath = dir </> (dropExtension name <> ".meta.json")
                sql <- BS.readFile sqlPath
                metaExists <- doesFileExist metaPath
                if not metaExists
                    then pure (Left ("missing metadata file: " <> metaPath))
                    else do
                        metaBytes <- BS.readFile metaPath
                        pure $
                            CorpusCase ("real_fuzz:" <> T.pack name)
                                <$> parseMetaDialect metaPath metaBytes
                                <*> Right sql
            pure (sequence results)

parseMetaDialect :: FilePath -> ByteString -> Either String SQLDialect
parseMetaDialect path bytes = do
    val <- eitherDecodeStrict' bytes
    case val of
        Object obj -> do
            let targetDbms = case KM.lookup "target" obj of
                    Just (Object tObj) -> case KM.lookup "dbms" tObj of
                        Just (String s) -> Just s
                        _ -> Nothing
                    _ -> Nothing
                topDialect = case KM.lookup "dialect" obj of
                    Just (String s) -> Just s
                    _ -> Nothing
                chosen = targetDbms <|> topDialect
            case chosen >>= lookupDialectByName of
                Just d -> Right d
                Nothing -> Left (path <> ": unrecognized dialect " <> show chosen)
        _ -> Left (path <> ": expected JSON object")

loadRegressionCases :: FilePath -> IO (Either String [CorpusCase])
loadRegressionCases path = do
    exists <- doesFileExist path
    if not exists
        then pure (Left ("missing regression cases file: " <> path))
        else parseRegressionCases path <$> BS.readFile path

parseRegressionCases :: FilePath -> ByteString -> Either String [CorpusCase]
parseRegressionCases path bytes = go (dropWhile (not . isCaseLine) (scanRawLines bytes))
    where
        total = BS.length bytes
        go [] = Right []
        go (hdr : rest) = do
            let (body, next) = break isCaseLine rest
                caseEnd = maybe total rawLineStart (listToMaybe next)
            case body of
                dialectLine : _ -> do
                    let cid = T.strip (decodeUtf8Lenient (BS.drop (BS.length "-- @case:") (rawLineContent hdr)))
                    dialects <- parseDialectHeader path dialectLine
                    let sqlStart = rawLineEnd dialectLine
                        rawSql = BS.take (caseEnd - sqlStart) (BS.drop sqlStart bytes)
                        sql = stripSingleTrailingLF rawSql
                        items = [CorpusCase cid d sql | d <- dialects]
                    when (null items) (Left (path <> ": empty dialect list for " <> T.unpack cid))
                    (items ++) <$> go next
                [] -> Left (path <> ":" <> show (rawLineNo hdr) <> ": missing -- @dialects: line")

data RawLine = RawLine
    { rawLineStart :: !Int
    , rawLineEnd :: !Int
    , rawLineContent :: !ByteString
    , rawLineNo :: !Int
    }

scanRawLines :: ByteString -> [RawLine]
scanRawLines bytes = go 0 1
    where
        !len = BS.length bytes
        go !off !no
            | off >= len = []
            | otherwise =
                let (chunk, rest) = BS.break (== 10) (BS.drop off bytes)
                    contentEnd = off + BS.length chunk
                    end = if BS.null rest then contentEnd else contentEnd + 1
                    content = if not (BS.null chunk) && BS.last chunk == 13 then BS.init chunk else chunk
                 in RawLine off end content no : go end (no + 1)

isCaseLine :: RawLine -> Bool
isCaseLine l = BS.isPrefixOf "-- @case:" (rawLineContent l)

parseDialectHeader :: FilePath -> RawLine -> Either String [SQLDialect]
parseDialectHeader path line =
    case BS.stripPrefix "-- @dialects:" (rawLineContent line) of
        Nothing -> Left (path <> ":" <> show (rawLineNo line) <> ": expected -- @dialects:")
        Just rest ->
            let names = map T.strip (T.splitOn "," (decodeUtf8Lenient (BSC.strip rest)))
             in traverse lookupOne names
    where
        lookupOne name =
            maybe
                (Left (path <> ":" <> show (rawLineNo line) <> ": unknown dialect " <> T.unpack name))
                Right
                (lookupDialectByName name)

stripSingleTrailingLF :: ByteString -> ByteString
stripSingleTrailingLF bs
    | not (BS.null bs) && BS.last bs == 10 = BS.init bs
    | otherwise = bs

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TE.decodeUtf8With (\_ _ -> Just '\xfffd')
