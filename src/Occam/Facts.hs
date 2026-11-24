module Occam.Facts (
    StatementUnit (..),
    StatementFacts (..),
    ParserEdit (..),
    ParserEditCategory (..),
    ColumnSite (..),
    ColumnSiteKind (..),
    ColumnTarget (..),
    editCategoryName,
    parseEditCategoryName,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, withText, (.!=), (.:), (.:?))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Occam.Core (Ident (..), Name (..), SemanticFacts (..), Statement (..))

-- | One parsed statement together with the facts the frontend extracted.
data StatementUnit = StatementUnit
    { unitStatement :: !Statement
    , unitFacts :: !(Maybe StatementFacts)
    , unitParseError :: !(Maybe Text)
    , unitCannotProduceResult :: !Bool
    }
    deriving (Eq, Show)

data StatementFacts = StatementFacts
    { factSemantics :: !SemanticFacts
    , factEdits :: ![ParserEdit]
    }
    deriving (Eq, Show)

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

editCategoryName :: ParserEditCategory -> Text
editCategoryName EditExpression = "expression"
editCategoryName EditClause = "clause"
editCategoryName EditConstraint = "constraint"
editCategoryName EditList = "list"
editCategoryName EditColumn = "column"
editCategoryName EditType = "type"
editCategoryName EditLiteral = "literal"

parseEditCategoryName :: Text -> Maybe ParserEditCategory
parseEditCategoryName "expression" = Just EditExpression
parseEditCategoryName "clause" = Just EditClause
parseEditCategoryName "constraint" = Just EditConstraint
parseEditCategoryName "list" = Just EditList
parseEditCategoryName "column" = Just EditColumn
parseEditCategoryName "type" = Just EditType
parseEditCategoryName "literal" = Just EditLiteral
parseEditCategoryName _ = Nothing

data ParserEdit = ParserEdit
    { parserEditStart :: !Int
    , parserEditEnd :: !Int
    , parserEditReplacement :: !ByteString
    , parserEditCategory :: !ParserEditCategory
    , parserEditLabel :: !Text
    , parserEditTopProjection :: !Bool
    }
    deriving (Eq, Show)

instance FromJSON ParserEdit where
    parseJSON = withObject "ParserEdit" \objectValue ->
        ParserEdit
            <$> objectValue .: "start"
            <*> objectValue .: "end"
            <*> (TE.encodeUtf8 <$> objectValue .: "replacement")
            <*> objectValue .: "category"
            <*> objectValue .: "label"
            <*> objectValue .:? "top_level_projection" .!= False

data ColumnSiteKind
    = CreateDefSite
    | InsertListSite
    | ValuesItemSite
    deriving (Eq, Ord, Show, Generic)

instance FromJSON ColumnSiteKind where
    parseJSON = withText "ColumnSiteKind" \name ->
        case name of
            "create_def" -> pure CreateDefSite
            "insert_list_item" -> pure InsertListSite
            "values_item" -> pure ValuesItemSite
            _ -> fail ("unknown column site kind: " <> T.unpack name)

data ColumnSite = ColumnSite
    { siteStmt :: !Int
    , siteStart :: !Int
    , siteEnd :: !Int
    , siteKind :: !ColumnSiteKind
    }
    deriving (Eq, Show, Generic)

instance FromJSON ColumnSite where
    parseJSON = withObject "ColumnSite" \objectValue ->
        ColumnSite
            <$> objectValue .: "stmt"
            <*> objectValue .: "start"
            <*> objectValue .: "end"
            <*> objectValue .: "kind"

-- | Column co-reduction target with sites and optional blocking reason.
data ColumnTarget = ColumnTarget
    { targetTable :: !Name
    , targetColumn :: !Ident
    , targetOrdinal :: !Int
    , targetSites :: ![ColumnSite]
    , targetBlocked :: !(Maybe Text)
    }
    deriving (Eq, Show, Generic)

instance FromJSON ColumnTarget where
    parseJSON = withObject "ColumnTarget" \objectValue ->
        ColumnTarget
            <$> objectValue .: "table"
            <*> objectValue .: "column"
            <*> objectValue .: "ordinal"
            <*> objectValue .: "sites"
            <*> objectValue .: "blocked"
