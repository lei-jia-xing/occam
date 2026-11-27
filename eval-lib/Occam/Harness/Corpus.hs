-- | Load SQL files and dialect metadata from the real fuzz corpus.
module Occam.Harness.Corpus (
    CorpusCase (..),
    loadRealFuzzCorpus,
    absolutePath,
    fileStem,
    normalizeDbms,
    validCaseId,
    uniqueCaseId,
    sqlSha256,
) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (IOException, try)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, runExceptT, throwE)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson (eitherDecodeStrict', withObject, (.:?))
import Data.Aeson.Types (parseEither)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Char (isAlphaNum, isAscii, ord)
import Data.Foldable qualified as Foldable
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Occam.Parser (SQLDialect, lookupDialectByName)
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath (
    hasDrive,
    joinPath,
    splitDirectories,
    splitDrive,
    takeBaseName,
    takeExtension,
    takeFileName,
    (</>),
 )

data CorpusCase = CorpusCase
    { corpusId :: !Text
    , corpusDialect :: !SQLDialect
    , corpusSql :: !ByteString
    }

loadRealFuzzCorpus :: FilePath -> IO (Either String [CorpusCase])
loadRealFuzzCorpus dir = do
    result <- try (runExceptT load) :: IO (Either IOException (Either String [CorpusCase]))
    pure (either (Left . show) id result)
    where
        load = do
            exists <- lift (doesDirectoryExist dir)
            unless exists (throwE ("missing real fuzz corpus directory: " <> dir))
            files <- lift (listDirectory dir)
            cases <- traverse loadCase (sort (filter ((== ".sql") . takeExtension) files))
            let caseIds = map corpusId cases
            unless (Set.size (Set.fromList caseIds) == length caseIds) $
                throwE ("duplicate case IDs in " <> dir)
            pure cases

        loadCase name = do
            let stem = fileStem name
                metaPath = dir </> (stem <> ".meta.json")
            meta <- lift (BS.readFile metaPath)
            (caseId, dialect) <- except (parseMetadata metaPath (T.pack stem) meta)
            sql <- lift (BS.readFile (dir </> name))
            pure (CorpusCase caseId dialect sql)

parseMetadata :: FilePath -> Text -> ByteString -> Either String (Text, SQLDialect)
parseMetadata path fallbackId bytes =
    first ((path <> ": ") <>) do
        val <- eitherDecodeStrict' bytes
        (caseId, name) <- parseEither (withObject "corpus metadata" parseFields) val
        dialect <- maybe (Left ("unrecognized dialect " <> show name)) Right (name >>= lookupDialectByName)
        pure (fromMaybe fallbackId caseId, dialect)
    where
        parseFields obj = do
            caseId <- obj .:? "id"
            target <- obj .:? "target"
            targetDbms <- maybe (pure Nothing) (withObject "target" (.:? "dbms")) target
            dialect <- obj .:? "dialect"
            pure (caseId, targetDbms <|> dialect)

-- | Keep the leading dots in names such as .sql, which have no extension.
fileStem :: FilePath -> FilePath
fileStem path =
    let stem = takeBaseName path
     in if all (== '.') stem then takeFileName path else stem

-- | Resolve dot segments lexically so paths and case IDs stay reproducible.
absolutePath :: FilePath -> IO FilePath
absolutePath path = do
    (drive, relative) <- splitDrive <$> makeAbsolute path
    pure (drive </> joinPath (reverse (Foldable.foldl' step [] (splitDirectories relative))))
    where
        step parts "." = parts
        step (_ : parts) ".." = parts
        step [] ".." = []
        step parts name = name : parts

normalizeDbms :: Text -> Text
normalizeDbms raw = case T.toLower (T.strip raw) of
    "postgresql" -> "postgres"
    "pgsql" -> "postgres"
    "sqlite3" -> "sqlite"
    other -> other

sqlSha256 :: ByteString -> Text
sqlSha256 = decodeUtf8 . Base16.encode . SHA256.hash

-- | Accept one literal path component for manifest and checkpoint IDs.
validCaseId :: Text -> Bool
validCaseId cid =
    not (T.null cid || hasDrive (T.unpack cid))
        && T.strip cid == cid
        && cid `notElem` [".", ".."]
        && not (T.any unsafe cid)
    where
        unsafe c = c == '/' || c == '\\' || ord c < 32

-- | Derive a filesystem-safe case ID and take the first available suffix.
uniqueCaseId :: Set.Set Text -> Text -> Text
uniqueCaseId used raw =
    let safe c = isAscii c && (isAlphaNum c || c `elem` ("_.-" :: String))
        normalized =
            T.concat [if T.any safe part then part else "_" | part <- T.groupBy (\a b -> safe a == safe b) raw]
        clean = T.dropAround (`elem` ("_.-" :: String)) normalized
        base = if T.null clean then "case" else clean
        candidate 1 = base
        candidate n = base <> "_" <> T.pack (show n)
     in candidate (until (\n -> Set.notMember (candidate n) used) (+ 1) (1 :: Integer))
