module Occam.Facts (
    StatementUnit (..),
    StatementFacts (..),
    ParserEdit (..),
    ParserEditCategory (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, withText, (.:))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Core (SemanticFacts (..), Statement (..))

-- | One parsed statement together with the facts the frontend extracted.
data StatementUnit = StatementUnit
    { unitStatement :: !Statement
    , unitFacts :: !(Maybe StatementFacts)
    , unitParseError :: !(Maybe Text)
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
    | EditType
    | EditLiteral
    deriving (Eq, Ord, Show)

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
editCategoryName EditType = "type"
editCategoryName EditLiteral = "literal"

parseEditCategoryName :: Text -> Maybe ParserEditCategory
parseEditCategoryName "expression" = Just EditExpression
parseEditCategoryName "clause" = Just EditClause
parseEditCategoryName "constraint" = Just EditConstraint
parseEditCategoryName "list" = Just EditList
parseEditCategoryName "type" = Just EditType
parseEditCategoryName "literal" = Just EditLiteral
parseEditCategoryName _ = Nothing

data ParserEdit = ParserEdit
    { parserEditStart :: !Int
    , parserEditEnd :: !Int
    , parserEditReplacement :: !ByteString
    , parserEditCategory :: !ParserEditCategory
    , parserEditLabel :: !Text
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
