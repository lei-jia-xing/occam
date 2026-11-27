module DependencyChecks.Support (
    factsFor,
    opsFor,
    ref,
    objectName,
    tableDefinition,
    definedColumns,
    isNamedIndex,
    readsFor,
    writesFor,
    allReadFacts,
    hasStatementRead,
    renamesFor,
    isAttach,
    isDetach,
    isAttachInherit,
    isDetachInherit,
    hasRead,
    hasWrite,
    readSources,
    isTruncateKind,
    defaultSessionInit,
    analyzeParsed,
    analyze,
    analyzeWith,
    dependenciesFor,
    has,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Occam.Core (
    CatalogOp (..),
    ColumnDef (..),
    Constraint (..),
    DataWrite (..),
    Ident (..),
    Name (..),
    NameRef (..),
    ReadFact (..),
    RenameOp (..),
    SemanticFacts (..),
    SessionInit (..),
    StatementId (..),
    TableDef (..),
    WriteKind (..),
    nameText,
 )
import Occam.Dependency (DependencyGraph (..), analyzeStatements)
import Occam.Parser (
    ParseOptions (..),
    ParsedSQL (..),
    SQLDialect (..),
    StatementUnit (..),
    defaultParseOptions,
    parseSQL,
 )
import Test.Tasty.HUnit (assertBool, assertFailure)
import TestSupport (emptySemanticFacts)

factsFor :: StatementId -> DependencyGraph -> SemanticFacts
factsFor sid graph = Map.findWithDefault emptySemanticFacts sid (graphFacts graph)

opsFor :: StatementId -> DependencyGraph -> [CatalogOp]
opsFor sid = factOps . factsFor sid

ref :: [Text] -> NameRef
ref parts = NameRef (Name (map Ident parts)) False

objectName :: [Text] -> Name
objectName = Name . map Ident

tableDefinition :: [Text] -> [CatalogOp] -> Maybe TableDef
tableDefinition parts ops =
    listToMaybe
        [ definition
        | DefineTableOp definition <- ops
        , tableName definition == objectName parts
        ]

definedColumns :: [Text] -> [CatalogOp] -> [Text]
definedColumns parts ops =
    maybe [] (map (identText . columnName) . tableColumns) (tableDefinition parts ops)

isNamedIndex :: Text -> Constraint -> Bool
isNamedIndex wanted (IndexConstraint (Just (Ident name)) _ _) = wanted == name
isNamedIndex _ _ = False

readsFor :: StatementId -> DependencyGraph -> [ReadFact]
readsFor sid graph = [readFact | ReadOp readFact <- opsFor sid graph]

writesFor :: StatementId -> DependencyGraph -> [DataWrite]
writesFor sid graph = [write | WriteDataOp write <- opsFor sid graph]

allReadFacts :: StatementId -> DependencyGraph -> [ReadFact]
allReadFacts sid graph =
    readsFor sid graph
        ++ concatMap dataWriteReads (writesFor sid graph)

hasStatementRead :: [Text] -> StatementId -> DependencyGraph -> Bool
hasStatementRead parts sid graph =
    any
        (\readFact -> readSource readFact == ref parts)
        (allReadFacts sid graph)

renamesFor :: StatementId -> DependencyGraph -> [RenameOp]
renamesFor sid graph = [rename | RenameOp rename <- opsFor sid graph]
isAttach :: CatalogOp -> Bool
isAttach (AttachPartitionOp _) = True
isAttach _ = False

isDetach :: CatalogOp -> Bool
isDetach (DetachPartitionOp _) = True
isDetach _ = False

isAttachInherit :: CatalogOp -> Bool
isAttachInherit (AttachInheritOp _) = True
isAttachInherit _ = False

isDetachInherit :: CatalogOp -> Bool
isDetachInherit (DetachInheritOp _) = True
isDetachInherit _ = False

hasRead :: [Text] -> [CatalogOp] -> Bool
hasRead parts ops =
    any
        (\readFact -> readSource readFact == ref parts)
        [readFact | ReadOp readFact <- ops]

hasWrite :: [Text] -> WriteKind -> [CatalogOp] -> Bool
hasWrite parts kind ops =
    any
        (\write -> dataWriteTarget write == ref parts && dataWriteKind write == kind)
        [write | WriteDataOp write <- ops]

readSources :: StatementId -> DependencyGraph -> Set.Set Text
readSources sid graph =
    Set.fromList
        [ nameText (nameRefName (readSource readFact))
        | readFact <- allReadFacts sid graph
        ]

isTruncateKind :: WriteKind -> Bool
isTruncateKind TruncateWrite {} = True
isTruncateKind _ = False

defaultSessionInit :: SessionInit
defaultSessionInit = SessionInit [] [] Nothing True True Map.empty False False True

analyzeParsed :: SQLDialect -> ByteString -> IO DependencyGraph
analyzeParsed dialect sql = do
    result <- parseSQL (defaultParseOptions dialect) sql
    case result of
        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
        Right parsed -> do
            assertBool
                ("expected supported " ++ show dialect ++ " syntax:\n" ++ BSC.unpack sql)
                (all (isJust . unitFacts) (parsedUnits parsed))
            pure (analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed))

analyze :: SQLDialect -> ByteString -> IO DependencyGraph
analyze dialect = analyzeWith (defaultParseOptions dialect)

analyzeWith :: ParseOptions -> ByteString -> IO DependencyGraph
analyzeWith options sql = do
    result <- parseSQL options sql
    case result of
        Left err -> assertFailure ("SQL parser transport error: " ++ show err)
        Right parsed ->
            pure (analyzeStatements (parsedInitialSession parsed) (parsedUnits parsed))

dependenciesFor :: StatementId -> DependencyGraph -> Set.Set StatementId
dependenciesFor sid graph = Map.findWithDefault Set.empty sid (graphEdges graph)

has :: StatementId -> Set.Set StatementId -> Bool
has = Set.member
