-- | Per-statement dependency semantics, structural parser edits, and column-analysis bundles.
module Occam.Facts (
    StatementUnit (..),
    StatementFacts (..),
    ParserEdit (..),
    ParserEditCategory (..),
    ClauseTier (..),
    ExprTier (..),
    EditTier (..),
    ColumnSite (..),
    ColumnSiteKind (..),
    ColumnTarget (..),
    editCategoryName,
    parseEditCategoryName,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Occam.Core.Schema (Ident (..), Name (..))
import Occam.Core.Semantics (SemanticFacts (..))
import Occam.Core.Types (Statement (..))

-- | One parsed statement together with the facts the frontend extracted.
data StatementUnit = StatementUnit
    { unitStatement :: !Statement
    , unitFacts :: !(Maybe StatementFacts)
    , unitParseError :: !(Maybe Text)
    , unitCannotProduceResult :: !Bool
    }
    deriving (Eq, Show)

-- | Per-statement dependency semantics and candidate syntax\/column rewrite edits.
data StatementFacts = StatementFacts
    { factSemantics :: !SemanticFacts
    , factEdits :: ![ParserEdit]
    }
    deriving (Eq, Show)

-- | Structural pass category ordering intra-statement parser edits during reduction.
data ParserEditCategory
    = EditExpression
    | EditClause
    | EditConstraint
    | EditList
    | EditColumn
    | EditType
    | EditLiteral
    deriving (Eq, Ord, Show, Generic)

instance ToJSON ParserEditCategory where
    toJSON category = toJSON (editCategoryName category)

instance FromJSON ParserEditCategory where
    parseJSON = withText "ParserEditCategory" \name ->
        maybe (fail ("unknown parser edit category: " <> T.unpack name)) pure (parseEditCategoryName name)

-- | Render a 'ParserEditCategory' as its lowercase wire\/JSON identifier.
editCategoryName :: ParserEditCategory -> Text
editCategoryName EditExpression = "expression"
editCategoryName EditClause = "clause"
editCategoryName EditConstraint = "constraint"
editCategoryName EditList = "list"
editCategoryName EditColumn = "column"
editCategoryName EditType = "type"
editCategoryName EditLiteral = "literal"

-- | Parse a lowercase wire\/JSON string into 'ParserEditCategory'.
parseEditCategoryName :: Text -> Maybe ParserEditCategory
parseEditCategoryName "expression" = Just EditExpression
parseEditCategoryName "clause" = Just EditClause
parseEditCategoryName "constraint" = Just EditConstraint
parseEditCategoryName "list" = Just EditList
parseEditCategoryName "column" = Just EditColumn
parseEditCategoryName "type" = Just EditType
parseEditCategoryName "literal" = Just EditLiteral
parseEditCategoryName _ = Nothing

-- | Priority tier for clause-removal edits (lower constructors run first).
data ClauseTier
    = SafeClause
    | StructuralClause
    | FragileClause
    deriving (Eq, Ord, Show, Generic)

-- | Priority tier for expression-simplification edits (lower constructors run first).
data ExprTier
    = UnwrapExpr
    | AggressiveExpr
    deriving (Eq, Ord, Show, Generic)

-- | Typed priority tier carried by 'ParserEdit' for bucket ordering without label matching.
data EditTier
    = TierDefault
    | TierClause !ClauseTier
    | TierExpr !ExprTier
    deriving (Eq, Ord, Show, Generic)

-- | Byte-span rewrite candidate emitted by the frontend AST analysis.
data ParserEdit = ParserEdit
    { parserEditStart :: !Int
    , parserEditEnd :: !Int
    , parserEditReplacement :: !ByteString
    , parserEditCategory :: !ParserEditCategory
    , parserEditTier :: !EditTier
    , parserEditLabel :: !Text
    , parserEditTopProjection :: !Bool
    , parserEditStringLiteral :: !Bool
    , parserEditRecursivePredicate :: !Bool
    , parserEditLowYield :: !Bool
    }
    deriving (Eq, Show)

-- | Role of a span within a column bundle (@CREATE@ definition, @INSERT@ column list, or @VALUES@ item).
data ColumnSiteKind
    = CreateDefSite
    | InsertListSite
    | ValuesItemSite
    deriving (Eq, Ord, Show, Generic)

-- | Source span corresponding to one coordinated site of a column deletion bundle.
data ColumnSite = ColumnSite
    { siteStmt :: !Int
    , siteStart :: !Int
    , siteEnd :: !Int
    , siteKind :: !ColumnSiteKind
    }
    deriving (Eq, Show, Generic)

-- | Column co-reduction target with sites and optional blocking reason.
data ColumnTarget = ColumnTarget
    { targetTable :: !Name
    , targetColumn :: !Ident
    , targetOrdinal :: !Int
    , targetSites :: ![ColumnSite]
    , targetBlocked :: !(Maybe Text)
    }
    deriving (Eq, Show, Generic)
