module Occam.Core (
    StatementId (..),
    Statement (..),
    Span (..),
) where

import Data.ByteString (ByteString)

newtype StatementId = StatementId Int
    deriving (Eq, Ord, Show)

data Span = Span
    { spanStartByte :: !Int
    , spanEndByte :: !Int
    }
    deriving (Eq, Show)

data Statement = Statement
    { stmtId :: !StatementId
    , stmtSpan :: !Span
    , stmtRaw :: !ByteString
    }
    deriving (Eq, Show)
