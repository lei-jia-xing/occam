-- | Seed-derived mutation properties and focused AST walker checks.
module FrontendSeedChecks.Mutation (
    propTailTruncation,
    propMiddleCorruption,
    propLexicalCorruption,
    focusedWalkerTests,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Maybe (isJust, listToMaybe)
import Data.Vector qualified as V
import FrontendAstInvariants (invariantViolations, validateStatementInvariants)
import FrontendSeedChecks.Corpus (contextLabel, seedLabel, sliceOrEmpty, spanWithin)
import FrontendSeedLoader (SeedCase (..))
import Occam.Frontend.Lexer (
    LexError (..),
    LexResult (..),
    SpannedToken,
    isTriviaToken,
    lexSQL,
 )
import Occam.Frontend.Parser (ParsedScript (..), parseScript, parseScriptUnits)
import Occam.Frontend.Span (
    ByteSpan (..),
    Located (..),
    isNonEmptyValidSpan,
    isUtf8Boundary,
    isValidSpan,
    spanEnd,
    spanIsEmpty,
    spanStart,
    unsafeSliceSpan,
 )
import Occam.Frontend.Splitter (SplitUnit (..), splitSQL)
import Occam.Parser (
    DialectProfile,
    LexicalProfile (..),
    SQLDialect (..),
    dialectLexical,
    dialectProfile,
 )
import Test.QuickCheck (Gen, Property, choose, counterexample, elements, forAllBlind)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)

-- The moved mutation properties follow.
data Mutation = Mutation
    { mutationName :: !String
    , mutationBytes :: !ByteString
    , mutationLexical :: !Bool
    }

data Lexed = Lexed
    { lexedTokens :: ![SpannedToken]
    , lexedError :: !(Maybe LexError)
    }

lexed :: DialectProfile -> ByteString -> Lexed
lexed profile sql =
    let LexResult tokens lexError' = lexSQL profile sql
     in Lexed (V.toList tokens) lexError'

tokenLayoutIssues :: String -> ByteString -> Lexed -> [String]
tokenLayoutIssues label sql lexedInput =
    [ label <> ": token span invalid " <> show (locSpan token)
    | token <- tokens
    , not (isNonEmptyValidSpan sql (locSpan token))
    ]
        ++ [ label <> ": tokens not contiguous at " <> show (locSpan right)
           | (left, right) <- zip tokens (drop 1 tokens)
           , spanEnd (locSpan left) /= spanStart (locSpan right)
           ]
        ++ [ label <> ": first token does not start at zero"
           | (firstToken : _) <- [tokens]
           , spanStart (locSpan firstToken) /= 0
           ]
        ++ case lexedError lexedInput of
            Nothing ->
                [ label <> ": token bytes do not cover the input"
                | BS.concat [unsafeSliceSpan (locSpan token) sql | token <- tokens] /= sql
                ]
                    ++ [ label <> ": last token does not end at EOF"
                       | (lastToken : _) <- [reverse tokens]
                       , spanEnd (locSpan lastToken) /= BS.length sql
                       ]
            Just err ->
                [ label <> ": lex error offset outside input"
                | lexErrorOffset err < 0 || lexErrorOffset err > BS.length sql
                ]
                    ++ [ label <> ": lex error offset is not a UTF-8 boundary"
                       | not (isUtf8Boundary sql (lexErrorOffset err))
                       ]
    where
        tokens = lexedTokens lexedInput

splitterLayoutIssues :: ByteString -> [SplitUnit] -> [String]
splitterLayoutIssues sql units =
    concatMap unitIssues units
        ++ [ "split units overlap at " <> show (splitUnitSpan right)
           | (left, right) <- zip units (drop 1 units)
           , spanStart (splitUnitSpan right) < spanEnd (splitUnitSpan left)
           ]
    where
        unitIssues unit =
            let unitSpan = splitUnitSpan unit
                hasLexError = isJust (splitUnitLexError unit)
             in [ "split unit span invalid: " <> show unitSpan
                | not (isValidSpan sql unitSpan)
                ]
                    ++ [ "empty split unit span: " <> show unitSpan
                       | not hasLexError && spanIsEmpty unitSpan
                       ]
                    ++ case splitUnitDelimiterSpan unit of
                        Nothing -> directiveIssues unitSpan (splitUnitDelimiterDirective unit)
                        Just delimiterSpan ->
                            [ "invalid statement delimiter span: " <> show delimiterSpan
                            | not (isNonEmptyValidSpan sql delimiterSpan)
                            ]
                                ++ [ "statement delimiter overlaps its unit: " <> show (unitSpan, delimiterSpan)
                                   | spanEnd unitSpan > spanStart delimiterSpan
                                   ]
        directiveIssues unitSpan = \case
            Nothing -> []
            Just (L directiveSpan _) ->
                [ "invalid directive span: " <> show directiveSpan
                | not (isNonEmptyValidSpan sql directiveSpan)
                ]
                    ++ [ "directive span is outside its unit: " <> show (directiveSpan, unitSpan)
                       | not (spanWithin directiveSpan unitSpan)
                       ]

prefixIssues ::
    DialectProfile ->
    ByteString ->
    String ->
    ByteString ->
    Int ->
    [SplitUnit] ->
    [String]
prefixIssues profile actualSql label prefixAnchor expectedCount actualUnits =
    case splitSQL profile prefixAnchor of
        Left err -> [label <> ": independently authored prefix failed to split: " <> show err]
        Right reference ->
            [ label
                <> ": prefix unit count short (actual "
                <> show (length actualUnits)
                <> ", expected "
                <> show expectedCount
                <> ")"
            | length actualUnits < expectedCount
            ]
                ++ [ label
                        <> ": reference prefix unit count differs (reference "
                        <> show (length reference)
                        <> ", expected "
                        <> show expectedCount
                        <> ")"
                   | length reference /= expectedCount
                   ]
                ++ [ label
                        <> ": prefix unit "
                        <> show index
                        <> " differs: actual "
                        <> show actualBytes
                        <> " reference "
                        <> show referenceBytes
                   | (index, (actual, referenceUnit)) <- indexedPairs (take expectedCount actualUnits) reference
                   , let actualBytes = sliceOrEmpty actualSql (splitUnitSpan actual)
                   , let referenceBytes = sliceOrEmpty prefixAnchor (splitUnitSpan referenceUnit)
                   , actualBytes /= referenceBytes || splitUnitSpan actual /= splitUnitSpan referenceUnit
                   ]

suffixIssues ::
    DialectProfile ->
    ByteString ->
    String ->
    ByteString ->
    Int ->
    Int ->
    [SplitUnit] ->
    [String]
suffixIssues profile actualSql label suffixAnchor suffixOffset expectedCount actualUnits =
    case splitSQL profile suffixAnchor of
        Left err -> [label <> ": independently authored suffix failed to split: " <> show err]
        Right reference ->
            [ label
                <> ": suffix unit count short (actual "
                <> show (length actualSuffix)
                <> ", expected "
                <> show expectedCount
                <> ")"
            | length actualSuffix < expectedCount
            ]
                ++ [ label
                        <> ": reference suffix unit count differs (reference "
                        <> show (length reference)
                        <> ", expected "
                        <> show expectedCount
                        <> ")"
                   | length reference /= expectedCount
                   ]
                ++ [ label
                        <> ": suffix unit "
                        <> show index
                        <> " differs: actual "
                        <> show actualBytes
                        <> " reference "
                        <> show referenceBytes
                   | (index, (actual, referenceUnit)) <- indexedPairs actualSuffix reference
                   , let actualBytes = sliceOrEmpty actualSql (splitUnitSpan actual)
                   , let referenceBytes = sliceOrEmpty suffixAnchor (splitUnitSpan referenceUnit)
                   , actualBytes /= referenceBytes
                   ]
                ++ [ label
                        <> ": suffix unit "
                        <> show index
                        <> " span differs: actual "
                        <> show (splitUnitSpan actual)
                        <> " expected "
                        <> show (shiftSpan suffixOffset (splitUnitSpan referenceUnit))
                   | (index, (actual, referenceUnit)) <- indexedPairs actualSuffix reference
                   , splitUnitSpan actual /= shiftSpan suffixOffset (splitUnitSpan referenceUnit)
                   ]
    where
        actualSuffix = drop (max 0 (length actualUnits - expectedCount)) actualUnits

indexedPairs :: [a] -> [b] -> [(Int, (a, b))]
indexedPairs left right = zip [0 :: Int ..] (zip left right)

shiftSpan :: Int -> ByteSpan -> ByteSpan
shiftSpan offset (ByteSpan start end) = ByteSpan (start + offset) (end + offset)

localLexIssues :: String -> ByteString -> Lexed -> [SplitUnit] -> [String]
localLexIssues label sql lexedInput units =
    case lexedError lexedInput of
        Nothing -> []
        Just err -> case reverse units of
            [] -> [label <> ": lexer reported an error but splitter produced no unit"]
            (lastUnit : _) ->
                [ label <> ": lex error was not attached to the last split unit"
                | splitUnitLexError lastUnit /= Just err
                ]
                    ++ [ label <> ": error unit does not reach EOF"
                       | spanEnd (splitUnitSpan lastUnit) /= BS.length sql
                       ]
                    ++ [ label <> ": lex error offset precedes its split unit"
                       | lexErrorOffset err < spanStart (splitUnitSpan lastUnit)
                       ]

expectNoLexIssues :: String -> Lexed -> [String]
expectNoLexIssues label lexedInput =
    case lexedError lexedInput of
        Nothing -> []
        Just err -> [label <> ": unexpected lex error " <> show err]

nonLexicalIssues :: String -> ByteString -> Lexed -> [SplitUnit] -> [String]
nonLexicalIssues label sql lexedInput units =
    case lexedError lexedInput of
        Just err -> [label <> ": non-lexical mutation unexpectedly produced " <> show err]
        Nothing -> case reverse units of
            [] -> [label <> ": no split units"]
            (lastUnit : _) ->
                [ label <> ": non-lexical trailing unit does not reach last substantive token"
                | spanEnd (splitUnitSpan lastUnit) /= lastSubstantiveEnd
                ]
                    ++ [ label <> ": non-lexical trailing unit unexpectedly terminated with a delimiter"
                       | isJust (splitUnitDelimiterSpan lastUnit)
                       ]
                    ++ [ label <> ": non-lexical trailing unit carries a lex error"
                       | isJust (splitUnitLexError lastUnit)
                       ]
    where
        lastSubstantiveEnd =
            maybe
                (BS.length sql)
                (spanEnd . locSpan)
                (listToMaybe (reverse (filter (not . isTriviaToken . locValue) (lexedTokens lexedInput))))

walkerIssues :: String -> ByteString -> DialectProfile -> [String]
walkerIssues label sql profile =
    case parseScriptUnits profile sql of
        Left err -> [label <> ": parseScriptUnits rejected valid UTF-8: " <> show err]
        Right parsedUnits ->
            [ label <> ": AST invariant: " <> violation
            | (_, Right statement) <- parsedUnits
            , violation <- invariantViolations sql statement
            ]

elementsNonEmpty :: String -> [a] -> Gen a
elementsNonEmpty label values = case values of
    [] -> error ("empty QuickCheck domain: " <> label)
    _ -> elements values

genCaseProfile :: String -> [SeedCase] -> Gen (SeedCase, DialectProfile)
genCaseProfile label cases = do
    seedCase <- elementsNonEmpty label cases
    profile <-
        elementsNonEmpty
            (label <> " dialects for " <> seedLabel seedCase)
            (map dialectProfile (seedCaseDialects seedCase))
    pure (seedCase, profile)

genTruncation :: [SeedCase] -> Gen (SeedCase, DialectProfile, Int, Int)
genTruncation cases = do
    (seedCase, profile) <- genCaseProfile "truncation" cases
    let sql = seedCaseSql seedCase
        segments = seedCaseSegments seedCase
        segmentSpans = seedCaseSegmentSpans seedCase
        boundaries segmentSpan first =
            [ offset
            | offset <- [spanStart segmentSpan + spanStart (locSpan first) + 1 .. spanEnd segmentSpan]
            , offset < BS.length sql
            , isUtf8Boundary sql offset
            ]
        candidates =
            [ (index, cuts)
            | (index, (segment, segmentSpan)) <- zip [1 ..] (drop 1 (zip segments segmentSpans))
            , Just first <- [listToMaybe (nonTriviaTokens profile segment)]
            , let cuts = boundaries segmentSpan first
            , not (null cuts)
            ]
    (segmentIndex, cuts) <- elementsNonEmpty "truncation segments" candidates
    cut <- elementsNonEmpty "UTF-8 truncation boundaries" cuts
    pure (seedCase, profile, segmentIndex, cut)

genMiddle :: [SeedCase] -> Gen (SeedCase, DialectProfile, Int, Mutation)
genMiddle cases = do
    (seedCase, profile) <- genCaseProfile "middle corruption" cases
    boundary <- choose (1, length (seedCaseUnits seedCase) - 1)
    mutation <- elements cleanMutations
    pure (seedCase, profile, boundary, mutation)

genLexical :: [SeedCase] -> Gen (SeedCase, DialectProfile, Int, Mutation)
genLexical cases = do
    (seedCase, profile) <- genCaseProfile "lexical corruption" cases
    boundary <- choose (1, length (seedCaseUnits seedCase) - 1)
    mutation <- elements (lexicalMutations profile)
    pure (seedCase, profile, boundary, mutation)

cleanMutations :: [Mutation]
cleanMutations =
    [ Mutation "syntax-noise" "SYNTAX ??? ERROR;" False
    , Mutation "unbalanced-parens" "SELECT ))) ???;" False
    , Mutation "quoted-noise" "SELECT 'x' ???;" False
    ]

lexicalMutations :: DialectProfile -> [Mutation]
lexicalMutations profile =
    [ Mutation "unterminated-single-quote" "SELECT 'unterminated" True
    , Mutation "unterminated-double-quote" "SELECT \"unterminated" True
    , Mutation "unterminated-block-comment" "SELECT /* unterminated" True
    , Mutation "unclosed-paren" "SELECT ((( ???" False
    ]
        ++ [ Mutation "unterminated-backtick" "SELECT `unterminated" True
           | lexBacktickQuotes (dialectLexical profile)
           ]
        ++ [ Mutation "unterminated-bracket" "SELECT [unterminated" True
           | lexBracketQuotes (dialectLexical profile)
           ]
        ++ [ Mutation "unterminated-dollar-string" "SELECT $tag$unterminated" True
           | lexDollarQuoting (dialectLexical profile)
           ]

nonTriviaTokens :: DialectProfile -> ByteString -> [SpannedToken]
nonTriviaTokens profile bytes =
    [ L sp token
    | L sp token <- V.toList (lexTokens (lexSQL profile bytes))
    , not (isTriviaToken token)
    ]

mutationContext :: SeedCase -> DialectProfile -> String -> String
mutationContext seedCase profile detail = contextLabel seedCase profile <> " " <> detail

propTailTruncation :: [SeedCase] -> Property
propTailTruncation cases =
    forAllBlind (genTruncation cases) $ \(seedCase, profile, segmentIndex, cut) ->
        let sql = seedCaseSql seedCase
            truncated = BS.take cut sql
            prefixAnchor = seedCasePrefixes seedCase !! (segmentIndex - 1)
            context =
                mutationContext
                    seedCase
                    profile
                    ("class=tail-truncation segment=" <> show segmentIndex <> " cut=" <> show cut)
            lexedInput = lexed profile truncated
            issues = case splitSQL profile truncated of
                Left err -> [context <> ": splitSQL rejected a UTF-8-safe truncation: " <> show err]
                Right units ->
                    splitterLayoutIssues truncated units
                        ++ prefixIssues profile truncated (context <> " prefix") prefixAnchor segmentIndex units
                        ++ tokenLayoutIssues (context <> " lexer") truncated lexedInput
                        ++ localLexIssues (context <> " lexer") truncated lexedInput units
                        ++ walkerIssues (context <> " AST") truncated profile
         in counterexample (context <> "\n" <> unlines issues) (null issues)

propMiddleCorruption :: [SeedCase] -> Property
propMiddleCorruption cases =
    forAllBlind (genMiddle cases) $ \(seedCase, profile, boundary, mutation) ->
        let prefixAnchor = seedCasePrefixes seedCase !! (boundary - 1)
            suffixAnchor = seedCaseSuffixes seedCase !! (boundary - 1)
            changes = mutationBytes mutation
            corrupt = prefixAnchor <> changes <> "\n" <> suffixAnchor
            suffixOffset = BS.length prefixAnchor + BS.length changes + 1
            unitCount = length (seedCaseUnits seedCase)
            context =
                mutationContext
                    seedCase
                    profile
                    ("class=" <> mutationName mutation <> " boundary=" <> show boundary)
            lexedInput = lexed profile corrupt
            issues = case splitSQL profile corrupt of
                Left err -> [context <> ": splitSQL rejected boundary-preserving corruption: " <> show err]
                Right units ->
                    splitterLayoutIssues corrupt units
                        ++ [ context <> ": expected " <> show (unitCount + 1) <> " units, got " <> show (length units)
                           | length units /= unitCount + 1
                           ]
                        ++ prefixIssues profile corrupt (context <> " prefix") prefixAnchor boundary units
                        ++ suffixIssues
                            profile
                            corrupt
                            (context <> " suffix")
                            suffixAnchor
                            suffixOffset
                            (unitCount - boundary)
                            units
                        ++ expectNoLexIssues (context <> " lexer") lexedInput
                        ++ tokenLayoutIssues (context <> " lexer") corrupt lexedInput
                        ++ walkerIssues (context <> " AST") corrupt profile
         in counterexample (context <> "\n" <> unlines issues) (null issues)

propLexicalCorruption :: [SeedCase] -> Property
propLexicalCorruption cases =
    forAllBlind (genLexical cases) $ \(seedCase, profile, boundary, mutation) ->
        let prefixAnchor = seedCasePrefixes seedCase !! (boundary - 1)
            suffixAnchor = seedCaseSuffixes seedCase !! (boundary - 1)
            corrupt = prefixAnchor <> mutationBytes mutation <> "\n" <> suffixAnchor
            context =
                mutationContext
                    seedCase
                    profile
                    ("class=" <> mutationName mutation <> " boundary=" <> show boundary)
            lexedInput = lexed profile corrupt
            issues = case splitSQL profile corrupt of
                Left err -> [context <> ": splitSQL returned Left for valid UTF-8 corruption: " <> show err]
                Right units ->
                    splitterLayoutIssues corrupt units
                        ++ prefixIssues profile corrupt (context <> " prefix") prefixAnchor boundary units
                        ++ tokenLayoutIssues (context <> " lexer") corrupt lexedInput
                        ++ if mutationLexical mutation
                            then localLexIssues (context <> " lexer") corrupt lexedInput units
                            else
                                nonLexicalIssues (context <> " lexer") corrupt lexedInput units
                                    ++ walkerIssues (context <> " AST") corrupt profile
         in counterexample (context <> "\n" <> unlines issues) (null issues)

-- ---------------------------------------------------------------------------
-- Focused in-memory walker coverage
-- ---------------------------------------------------------------------------

focusedWalkerTests :: TestTree
focusedWalkerTests =
    testGroup
        "located COPY/EXPLAIN payloads"
        [ testCase "COPY bare option span includes its optional value" $ do
            let sql = "COPY seed_copy FROM stdin WITH NULL '\\N'"
            assertSingleStatementValid "COPY" (dialectProfile PostgreSQL) sql
        , testCase "EXPLAIN option name and quoted literal stay source-exact" $ do
            let sql = "EXPLAIN (FORMAT 'json') SELECT 1"
            assertSingleStatementValid "EXPLAIN" (dialectProfile PostgreSQL) sql
        ]

assertSingleStatementValid :: String -> DialectProfile -> ByteString -> Assertion
assertSingleStatementValid label profile sql =
    case parseScript profile sql of
        Left err -> assertFailure (label <> ": parse failed: " <> show err)
        Right (ParsedScript statements) -> do
            assertEqual (label <> ": statement count") 1 (length statements)
            for_ statements (validateStatementInvariants sql)
