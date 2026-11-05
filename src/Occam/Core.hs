module Occam.Core (
    StatementId (..),
    Statement (..),
    Span (..),
    CrashSignature (..),
) where

import Data.ByteString (ByteString)
import Data.Text (Text)

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

data CrashSignature = CrashSignature
    { crashSignal :: !Int
    , crashFrames :: ![Text]
    }
    deriving (Eq, Show)
