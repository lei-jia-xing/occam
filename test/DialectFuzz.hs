-- | Statement-boundary and reducer properties for every product dialect.
module DialectFuzz (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Occam.Frontend.Parser (ParsedScript (..), parseScript)
import Occam.Parser (
    DialectProfile (..),
    SQLDialect (..),
    allDialectProfiles,
    countStatements,
 )
import Occam.Pool (withPredicatePool)
import Occam.Reducer (reduceSQL)
import Test.QuickCheck (
    Gen,
    Property,
    chooseInt,
    conjoin,
    counterexample,
    elements,
    forAllShrink,
    ioProperty,
    shrinkList,
    vectorOf,
    (===),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestSupport (checkParserValidity)

tests :: TestTree
tests =
    testGroup
        "Six-dialect fuzz"
        [ testProperty (T.unpack (dialectCanonicalName profile)) (reductionProperty profile)
        | profile <- allDialectProfiles
        ]

data Script = Script
    { required :: ![ByteString]
    , distractors :: ![ByteString]
    , prefix :: !ByteString
    }
    deriving (Show)

generateScript :: SQLDialect -> Gen Script
generateScript dialect = do
    n <- BSC.pack . show <$> chooseInt (1, 999)
    noiseCount <- chooseInt (1, 12)
    let table = quoteName dialect ("fuzz_" <> n <> ";table")
        engine = case dialect of
            ClickHouse -> " ENGINE = Memory"
            _ -> ""
        create = "CREATE TABLE " <> table <> " (id INT)" <> engine <> ";"
        insert = "INSERT INTO " <> table <> " (id) VALUES (" <> n <> ");"
        select = case dialect of
            PostgreSQL -> "SELECT DISTINCT ON (id) id FROM " <> table <> " ORDER BY id;"
            SQLite -> "SELECT id FROM " <> table <> " ORDER BY id LIMIT 0, 1;"
            DuckDB -> "SELECT id FROM " <> table <> " QUALIFY row_number() OVER () = 1;"
            ClickHouse -> "SELECT id FROM " <> table <> " PREWHERE id > 0 SETTINGS max_threads = 1;"
            MySQL -> "SELECT id FROM " <> table <> " ORDER BY id LIMIT 0, 1;"
            MariaDB -> "SELECT id FROM " <> table <> " ORDER BY id LIMIT 1 OFFSET 0;"
    noise <- vectorOf noiseCount $ do
        padding <- elements ["", "  ", "/* ; */ ", "\t"]
        literal <- elements ["0", "42", "'distractor;''quoted'", "'semi;colon'"]
        pure (padding <> "SELECT " <> literal <> " AS fuzz_noise;")
    leading <- elements ["", "-- ;\n", encodeUtf8 "/* 注释 ; café */\r\n"]
    pure (Script [create, insert, select] noise leading)

quoteName :: SQLDialect -> ByteString -> ByteString
quoteName SQLite name = "[" <> name <> "]"
quoteName MySQL name = "`" <> name <> "`"
quoteName MariaDB name = "`" <> name <> "`"
quoteName _ name = "\"" <> name <> "\""

reductionProperty :: DialectProfile -> Property
reductionProperty profile =
    forAllShrink (generateScript dialect) shrinkScript $ \script -> ioProperty $ do
        let requiredSql = required script
            noise = distractors script
            input = prefix script <> BSC.unlines (noise ++ requiredSql)
            predicate candidate = pure (all (`BS.isInfixOf` candidate) requiredSql)
            reduce inputSql = withPredicatePool 1 False predicate (\pool -> reduceSQL dialect pool inputSql)
        reduced <- reduce input
        reducedAgain <- reduce reduced
        inputValid <- checkParserValidity dialect input
        outputValid <- checkParserValidity dialect reduced
        count <- countStatements dialect input
        pure $
            counterexample ("Input:\n" <> BSC.unpack input <> "\nReduced:\n" <> BSC.unpack reduced) $
                conjoin
                    [ count === Right (length noise + length requiredSql)
                    , fmap (length . parsedScriptStatements) (parseScript profile input)
                        === Right (length noise + length requiredSql)
                    , inputValid === True
                    , outputValid === True
                    , all (`BS.isInfixOf` reduced) requiredSql === True
                    , BS.isInfixOf "fuzz_noise" reduced === False
                    , (BS.length reduced <= BS.length input) === True
                    , reducedAgain === reduced
                    ]
    where
        dialect = dialectId profile
        shrinkScript script = [script {distractors = noise} | noise <- shrinkList (const []) (distractors script)]
