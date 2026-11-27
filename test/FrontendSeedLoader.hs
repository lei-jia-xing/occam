{- | Loader for the checked frontend seed corpus (test/seeds/*.sql).

The loader is intentionally independent of the splitter and parser: it only
understands the @-- @case@, @-- @dialects@, @-- @units@ headers and the
exact @-- @unit@ boundary comment lines.  SQL bytes are never rewritten.
-}
module FrontendSeedLoader (
    SeedCase (..),
    UnitExpectation (..),
    seedDirectory,
    seedFileNames,
    seedCorpusPath,
    loadSeedCorpus,
    loadSeedFile,
    expectedUnitSqlOnlyCount,
    expectedUnitDirective,
    activeDelimiterAfter,
) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Frontend.Span (ByteSpan (..))
import Occam.Parser (DialectProfile (..), SQLDialect, allDialectProfiles)
import System.FilePath ((</>))

-- | Exact expected terminator for one independently split unit.
data UnitExpectation
    = ExpectSql !ByteString
    | ExpectDirective !ByteString
    | ExpectNoTerminator
    deriving (Eq, Show)

data SeedCase = SeedCase
    { seedCaseFile :: !FilePath
    , seedCaseLine :: !Int
    , seedCaseId :: !Text
    , seedCaseDialects :: ![SQLDialect]
    , seedCaseUnits :: ![UnitExpectation]
    , seedCaseSql :: !ByteString
    , seedCaseSegments :: ![ByteString]
    , seedCaseSegmentSpans :: ![ByteSpan]
    , seedCasePrefixes :: ![ByteString]
    , seedCaseSuffixes :: ![ByteString]
    }
    deriving (Eq, Show)

seedDirectory :: FilePath
seedDirectory = "test" </> "seeds"

seedFileNames :: [FilePath]
seedFileNames =
    [ "postgresql.sql"
    , "mysql.sql"
    , "mariadb.sql"
    , "sqlite.sql"
    , "duckdb.sql"
    , "clickhouse.sql"
    , "invalid.sql"
    ]

seedCorpusPath :: FilePath -> FilePath
seedCorpusPath name = seedDirectory </> name

-- | Load every required seed file.  Missing files are reported, not skipped.
loadSeedCorpus :: IO (Either String [SeedCase])
loadSeedCorpus = go [] seedFileNames
    where
        go acc [] = pure (validateCaseIds (reverse acc))
        go acc (name : rest) = do
            loaded <- loadSeedFile (seedCorpusPath name)
            case loaded of
                Left err -> pure (Left err)
                Right cases -> go (reverse cases ++ acc) rest

loadSeedFile :: FilePath -> IO (Either String [SeedCase])
loadSeedFile path = do
    bytes <- BS.readFile path
    pure (parseSeedFile path bytes)

parseSeedFile :: FilePath -> ByteString -> Either String [SeedCase]
parseSeedFile path bytes =
    let leading = dropWhile (not . isCaseLine) (scanLines bytes)
     in if null leading
            then Left (path <> ": no -- @case: blocks found")
            else go leading
    where
        total = BS.length bytes
        go [] = Right []
        go (headerLine : rest) = do
            let (body, next) = break isCaseLine rest
                caseEnd = maybe total lineStart (listToMaybe next)
            seedCase <- parseCaseBlock path bytes caseEnd headerLine body
            cases <- go next
            pure (seedCase : cases)

type HeaderBlock = (ByteString, ByteString, Int)

parseCaseBlock :: FilePath -> ByteString -> Int -> Line -> [Line] -> Either String SeedCase
parseCaseBlock path bytes nextCaseStart headerLine body = do
    let rawId = caseId headerLine
    when (T.null rawId) (Left (at headerLine "empty -- @case: id"))
    (dialectsRaw, unitsRaw, headerEnd) <- parseHeaders path headerLine body
    dialects <- parseDialects headerLine dialectsRaw
    units <- parseUnits headerLine unitsRaw
    let caseEnd = nextCaseStart
        sql = BS.take (caseEnd - headerEnd) (BS.drop headerEnd bytes)
        markerLines = [l | l <- scanLines sql, lineBytes l == "-- @unit"]
        markerEnds = map lineEnd markerLines
        segmentStarts = 0 : markerEnds
        segmentEnds = map lineStart markerLines ++ [BS.length sql]
        segments =
            zipWith
                (\lo hi -> BS.take (hi - lo) (BS.drop lo sql))
                segmentStarts
                segmentEnds
    rejectLateHeaders (scanLines sql)
    if length markerLines /= length units - 1
        then
            Left
                ( at
                    headerLine
                    ( "expected "
                        <> show (length units - 1)
                        <> " -- @unit boundaries for "
                        <> show (length units)
                        <> " units, found "
                        <> show (length markerLines)
                    )
                )
        else
            if 0 `elem` zipWith (-) segmentEnds segmentStarts
                then
                    Left (at headerLine "empty expected unit segment; -- @unit lines must separate non-empty units")
                else
                    Right
                        SeedCase
                            { seedCaseFile = path
                            , seedCaseLine = lineNumber headerLine
                            , seedCaseId = rawId
                            , seedCaseDialects = dialects
                            , seedCaseUnits = units
                            , seedCaseSql = sql
                            , seedCaseSegments = segments
                            , seedCaseSegmentSpans = zipWith ByteSpan segmentStarts segmentEnds
                            , seedCasePrefixes = map (`BS.take` sql) markerEnds
                            , seedCaseSuffixes = map (`BS.drop` sql) markerEnds
                            }
    where
        at line msg =
            path <> ":" <> show (lineNumber line) <> ": " <> msg
        rejectLateHeaders = mapM_ \line ->
            case headerHeaderName (lineBytes line) of
                Just name ->
                    Left (at headerLine ("duplicate or late " <> name <> " header after SQL started"))
                Nothing -> Right ()

parseHeaders :: FilePath -> Line -> [Line] -> Either String HeaderBlock
parseHeaders path headerLine (dialectLine : unitLine : _) = do
    dialects <- readHeader "-- @dialects:" dialectLine
    units <- readHeader "-- @units:" unitLine
    pure (dialects, units, lineEnd unitLine)
    where
        readHeader prefix line =
            maybe
                (Left (path <> ":" <> show (lineNumber headerLine) <> ": expected " <> BSC.unpack prefix))
                Right
                (headerValue prefix (lineBytes line))
parseHeaders path headerLine _ =
    Left (path <> ":" <> show (lineNumber headerLine) <> ": incomplete seed metadata")

headerHeaderName :: ByteString -> Maybe String
headerHeaderName line
    | BS.isPrefixOf "-- @dialects:" line = Just "-- @dialects:"
    | BS.isPrefixOf "-- @units:" line = Just "-- @units:"
    | otherwise = Nothing

parseDialects :: Line -> ByteString -> Either String [SQLDialect]
parseDialects line raw =
    case traverse parseOne entries of
        Left msg -> Left (location <> ": " <> msg)
        Right [] -> Left (location <> ": empty -- @dialects: list")
        Right ds
            | length ds /= length (dedupe ds) -> Left (location <> ": duplicate dialect in -- @dialects:")
            | otherwise -> Right ds
    where
        location = "seed case at line " <> show (lineNumber line)
        entries = map T.strip (T.splitOn "," (decode raw))
        parseOne name
            | T.null name = Left "empty dialect entry in -- @dialects:"
            | otherwise =
                case [dialectId p | p <- allDialectProfiles, dialectCanonicalName p == name] of
                    [d] -> Right d
                    _ ->
                        Left
                            ( "unknown dialect "
                                <> T.unpack name
                                <> "; use canonical names only: "
                                <> T.unpack (T.intercalate ", " (map dialectCanonicalName allDialectProfiles))
                            )
        dedupe = Foldable.foldl' (\acc d -> if d `elem` acc then acc else acc ++ [d]) []

parseUnits :: Line -> ByteString -> Either String [UnitExpectation]
parseUnits line raw =
    case traverse (parseOne . T.strip) (T.splitOn "," (decode raw)) of
        Left msg -> Left (location <> ": " <> msg)
        Right [] -> Left (location <> ": empty -- @units: list")
        Right units -> Right units
    where
        location = "seed case at line " <> show (lineNumber line)
        parseOne text
            | T.null text = Left "empty unit entry in -- @units:"
            | text == "sql" = Right ExpectNoTerminator
            | text == "directive" =
                Left "bare 'directive' is not canonical; write directive=<exact argument>"
            | Just arg <- T.stripPrefix "directive=" text
            , not (T.null arg) =
                Right (ExpectDirective (TE.encodeUtf8 arg))
            | Just delimiter <- T.stripPrefix "sql=" text
            , not (T.null delimiter) =
                Right (ExpectSql (TE.encodeUtf8 delimiter))
            | otherwise =
                Left ("invalid unit expectation " <> show (T.unpack text))

validateCaseIds :: [SeedCase] -> Either String [SeedCase]
validateCaseIds cases = do
    _ <- Foldable.foldl' check (Right Map.empty) cases
    pure cases
    where
        check (Left err) _ = Left err
        check (Right seen) seedCase =
            let ident = seedCaseId seedCase
             in case Map.lookup ident seen of
                    Nothing -> Right (Map.insert ident (seedCaseFile seedCase, seedCaseLine seedCase) seen)
                    Just (firstFile, firstLine) ->
                        Left
                            ( seedCaseFile seedCase
                                <> ":"
                                <> show (seedCaseLine seedCase)
                                <> ": duplicate global case id "
                                <> T.unpack ident
                                <> " (first at "
                                <> firstFile
                                <> ":"
                                <> show firstLine
                                <> ")"
                            )

expectedUnitSqlOnlyCount :: SeedCase -> Int
expectedUnitSqlOnlyCount =
    length . filter (\case ExpectDirective _ -> False; _ -> True) . seedCaseUnits

expectedUnitDirective :: UnitExpectation -> Maybe ByteString
expectedUnitDirective = \case
    ExpectDirective delimiter -> Just delimiter
    _ -> Nothing

{- | Active client delimiter after each unit.  Only @DELIMITER@ directives
change the splitter state; ordinary statement terminators never do.
-}
activeDelimiterAfter :: SeedCase -> [ByteString]
activeDelimiterAfter seedCase = snd (Foldable.foldl' step (";", []) (seedCaseUnits seedCase))
    where
        step (active, acc) = \case
            ExpectDirective delimiter -> (delimiter, acc ++ [delimiter])
            ExpectSql _ -> (active, acc ++ [active])
            ExpectNoTerminator -> (active, acc ++ [active])

-- ---------------------------------------------------------------------------
-- Raw line scanning
-- ---------------------------------------------------------------------------

data Line = Line
    { lineStart :: !Int
    , lineEnd :: !Int
    , lineBytes :: !ByteString
    , lineNumber :: !Int
    }

scanLines :: ByteString -> [Line]
scanLines bytes = go 0 1
    where
        !len = BS.length bytes
        go !offset !number
            | offset >= len = []
            | otherwise =
                let (chunk, rest) = BS.break (== 10) (BS.drop offset bytes)
                    contentEnd = offset + BS.length chunk
                    end = if BS.null rest then contentEnd else contentEnd + 1
                    content = dropCarriageReturn chunk
                 in Line offset end content number : go end (number + 1)

dropCarriageReturn :: ByteString -> ByteString
dropCarriageReturn bs
    | not (BS.null bs) && BS.last bs == 13 = BS.init bs
    | otherwise = bs

isCaseLine :: Line -> Bool
isCaseLine line = BS.isPrefixOf "-- @case:" (lineBytes line)

caseId :: Line -> Text
caseId = T.strip . decode . BS.drop (BS.length "-- @case:") . lineBytes

headerValue :: ByteString -> ByteString -> Maybe ByteString
headerValue prefix line = case BS.stripPrefix prefix line of
    Nothing -> Nothing
    Just rest -> Just (BSC.strip rest)

decode :: ByteString -> Text
decode = TE.decodeUtf8With (\_ _ -> Just '\xfffd')
