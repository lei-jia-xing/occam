-- | Static barrier and dependency counts for a corpus of SQL fixtures.
module Occam.Harness.BarrierCensus (barrierCensus) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Core (BarrierScope (..), ScopedBarrier (..), SemanticFacts (..))
import Occam.Dependency (DependencyGraph (..), analyzeStatements)
import Occam.Harness.Corpus (CorpusCase (..), loadRealFuzzCorpus)
import Occam.Parser (
    ParsedSQL (..),
    StatementUnit (..),
    defaultParseOptions,
    dialectName,
    parseSQL,
 )

data CaseCensus = CaseCensus
    { caseId :: !Text
    , caseDialect :: !Text
    , caseStatements :: !Int
    , caseSyntaxErrors :: !Int
    , caseHasGlobalFence :: !Bool
    , caseBarrierStatements :: !Int
    , caseBarrierActions :: !Int
    , caseBarrierEdges :: !Int
    , caseDeclaredByReason :: !(Map Text Int)
    , caseDeclaredByScope :: !(Map Text Int)
    }

instance ToJSON CaseCensus where
    toJSON row =
        object
            [ "case_id" .= caseId row
            , "dialect" .= caseDialect row
            , "statements" .= caseStatements row
            , "syntax_errors" .= caseSyntaxErrors row
            , "has_global_fence" .= caseHasGlobalFence row
            , "barrier_statements" .= caseBarrierStatements row
            , "barrier_actions" .= caseBarrierActions row
            , "barrier_edges" .= caseBarrierEdges row
            , "declared_by_reason" .= caseDeclaredByReason row
            , "declared_by_scope" .= caseDeclaredByScope row
            ]

-- | Analyze the corpus with the production frontend and dependency model.
barrierCensus :: FilePath -> IO (Either String Value)
barrierCensus dir = runExceptT do
    cases <- ExceptT (loadRealFuzzCorpus dir)
    rows <- traverse analyzeCase cases
    let byDialect = Map.fromListWith (<>) [(caseDialect row, [row]) | row <- rows]
    pure $
        object
            [ "schema_version" .= (1 :: Int)
            , "overall" .= summarize rows
            , "by_dialect" .= Map.map summarize byDialect
            , "cases" .= rows
            ]

analyzeCase :: CorpusCase -> ExceptT String IO CaseCensus
analyzeCase item = do
    let dialect = corpusDialect item
    result <- lift (parseSQL (defaultParseOptions dialect) (corpusSql item))
    parsed <- except (first (((T.unpack (corpusId item) <> ": ") <>) . show) result)
    let units = parsedUnits parsed
        graph = analyzeStatements (parsedInitialSession parsed) units
        barriers = concatMap factBarriers (graphFacts graph)
        histogram key = Map.fromListWith (+) [(key barrier, 1) | barrier <- barriers]
    pure
        CaseCensus
            { caseId = corpusId item
            , caseDialect = dialectName dialect
            , caseStatements = length units
            , caseSyntaxErrors = length (filter (isJust . unitParseError) units)
            , caseHasGlobalFence = graphHasGlobalBarrier graph
            , caseBarrierStatements = graphBarrierStatements graph
            , caseBarrierActions = graphBarrierActions graph
            , caseBarrierEdges = graphBarrierEdges graph
            , caseDeclaredByReason = histogram (T.pack . show . barrierReason)
            , caseDeclaredByScope = histogram (scopeName . barrierScope)
            }

scopeName :: BarrierScope -> Text
scopeName = \case
    GlobalScope -> "global"
    RelationScope _ -> "relation"
    SchemaScope _ -> "schema"
    ObjectScope _ -> "object"
    SessionStateScope -> "session"
    TransactionStateScope -> "transaction"
    UserVariableScope _ -> "user_variable"

summarize :: [CaseCensus] -> Value
summarize rows =
    object
        [ "case_count" .= length rows
        , "statements" .= sum (map caseStatements rows)
        , "syntax_errors" .= sum (map caseSyntaxErrors rows)
        , "cases_with_global_fences" .= length (filter caseHasGlobalFence rows)
        , "cases_without_barrier_statements" .= length (filter ((== 0) . caseBarrierStatements) rows)
        , "barrier_statements" .= sum (map caseBarrierStatements rows)
        , "barrier_actions" .= sum (map caseBarrierActions rows)
        , "barrier_edges" .= sum (map caseBarrierEdges rows)
        , "declared_by_reason" .= Map.unionsWith (+) (map caseDeclaredByReason rows)
        , "declared_by_scope" .= Map.unionsWith (+) (map caseDeclaredByScope rows)
        ]
