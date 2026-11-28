-- | Shared expression, literal, name, and scalar-type syntax nodes.
module Occam.Frontend.AST.Expr (
    NamePart (..),
    Name (..),
    Alias (..),
    Literal (..),
    ParamKind (..),
    Expr (..),
    UnaryOp (..),
    BinaryOp (..),
    LikeOp (..),
    Quantifier (..),
    CastStyle (..),
    FunctionCall (..),
    NullTreatment (..),
    FunctionArg (..),
    CaseExpr (..),
    WindowRef (..),
    WindowSpec (..),
    WindowFrame (..),
    WindowFrameUnit (..),
    WindowBound (..),
    OrderItem (..),
    SortDirection (..),
    NullsOrder (..),
    TypeName (..),
    TypeArg (..),
    IndexKeyPart (..),
) where

import Data.ByteString (ByteString)
import {-# SOURCE #-} Occam.Frontend.AST.Query (Query)
import Occam.Frontend.Lexer (LiteralKind (..))
import Occam.Frontend.Span (Located)

-- ---------------------------------------------------------------------------
-- Names, literals, and parameters
-- ---------------------------------------------------------------------------

{- | One identifier component.  'namePartRaw' is the exact source spelling
(quotes included when quoted); 'namePartQuoted' distinguishes quoted
identifiers from unquoted keywords/identifiers for case folding.
-}
data NamePart = NamePart
    { namePartRaw :: !ByteString
    , namePartQuoted :: !Bool
    }
    deriving (Eq, Ord, Show)

-- | A possibly schema-qualified object or column name.
newtype Name = Name
    { nameParts :: [Located NamePart]
    }
    deriving (Eq, Ord, Show)

-- | Table / subquery alias, optionally with a column-name list.
data Alias = Alias
    { aliasName :: !(Located NamePart)
    , aliasColumns :: ![Located NamePart]
    }
    deriving (Eq, Ord, Show)

-- | Literal token with its original source spelling.
data Literal = Literal
    { literalKind :: !LiteralKind
    , literalRaw :: !ByteString
    }
    deriving (Eq, Ord, Show)

-- | Parameter or variable class.
data ParamKind
    = ParamPositional
    | ParamUser
    | ParamSystem
    deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Expressions
-- ---------------------------------------------------------------------------

-- | Expression tree; spans cover the full expression text.
data Expr
    = ExprLiteral !Literal
    | ExprNull
    | ExprBoolean !Bool
    | ExprColumn !(Located Name)
    | ExprStar
    | ExprQualifiedStar !(Located Name)
    | ExprParam !ParamKind !ByteString
    | ExprDefault
    | ExprUnary !UnaryOp !(Located Expr)
    | ExprBinary !BinaryOp !(Located Expr) !(Located Expr)
    | ExprCast !CastStyle !(Located Expr) !(Located TypeName)
    | ExprCastExpr !(Located Expr) !(Located Expr)
    | ExprLambda ![Located NamePart] !(Located Expr)
    | ExprCharsetString !ByteString !(Located Literal)
    | ExprTypedString !ByteString !(Located Literal)
    | ExprSubscript
        !(Located Expr)
        !(Maybe (Located Expr))
        !(Maybe (Located Expr))
    | ExprField !(Located Expr) !(Located NamePart)
    | ExprFieldIndex !(Located Expr) !(Located ByteString)
    | ExprFieldStar !(Located Expr)
    | ExprCall !(Located FunctionCall)
    | ExprCase !CaseExpr
    | ExprExists !(Located Query)
    | ExprIsNull !Bool !(Located Expr)
    | ExprIsTrue !Bool !(Located Expr)
    | ExprIsFalse !Bool !(Located Expr)
    | ExprIsUnknown !Bool !(Located Expr)
    | ExprIsDistinct !Bool !(Located Expr) !(Located Expr)
    | ExprBetween !Bool !(Located Expr) !(Located Expr) !(Located Expr)
    | ExprInList !Bool !(Located Expr) ![Located Expr]
    | ExprInSubquery !Bool !(Located Expr) !(Located Query)
    | ExprLike !Bool !LikeOp !(Located Expr) !(Located Expr) !(Maybe (Located Expr))
    | ExprQuantified
        !Quantifier
        !BinaryOp
        !(Located Expr)
        !(Located Expr)
    | ExprArray ![Located Expr]
    | ExprStruct ![(Located Expr, Located Expr)]
    | ExprBareKeyword !(Located ByteString)
    | ExprTuple ![Located Expr]
    | ExprSubquery !(Located Query)
    | ExprParen !(Located Expr)
    | ExprCollate !(Located Expr) !(Located Name)
    deriving (Eq, Show)

data UnaryOp
    = UnaryNot
    | UnaryNegate
    | UnaryPositive
    | UnaryBitNot
    deriving (Eq, Ord, Show)

data BinaryOp
    = OpOr
    | OpAnd
    | OpXor
    | OpEq
    | OpNotEq
    | OpLt
    | OpLtEq
    | OpGt
    | OpGtEq
    | OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpMod
    | OpPow
    | OpConcat
    | OpBitAnd
    | OpBitOr
    | OpBitXor
    | OpShiftLeft
    | OpShiftRight
    | OpJsonGet
    | OpJsonGetText
    | OpJsonPathGet
    | OpJsonPathGetText
    | OpContains
    | OpContainedBy
    | OpOverlap
    | OpRegexMatch
    | OpRegexIMatch
    | OpRegexNotMatch
    | OpRegexNotIMatch
    | OpCustom !ByteString
    deriving (Eq, Ord, Show)

data LikeOp
    = LikeLike
    | LikeILike
    | LikeGlob
    | LikeRegexp
    deriving (Eq, Ord, Show)

data Quantifier
    = QuantAny
    | QuantAll
    | QuantSome
    deriving (Eq, Ord, Show)

data CastStyle
    = CastDoubleColon
    | CastDotColon
    | CastFunction
    | CastTryFunction
    deriving (Eq, Ord, Show)

-- | Function call with aggregate/window decorations.
data FunctionCall = FunctionCall
    { functionName :: !(Located Name)
    , functionDistinct :: !Bool
    , functionArgs :: ![Located FunctionArg]
    , functionOrderBy :: ![Located OrderItem]
    , functionWithinGroup :: ![Located OrderItem]
    , functionFilter :: !(Maybe (Located Expr))
    , functionNullTreatment :: !(Maybe NullTreatment)
    , functionOver :: !(Maybe WindowRef)
    }
    deriving (Eq, Show)

data NullTreatment
    = NullTreatmentRespect
    | NullTreatmentIgnore
    deriving (Eq, Ord, Show)

data FunctionArg
    = FunctionArgExpr !(Located Expr)
    | FunctionArgStar
    | FunctionArgNamed !(Located NamePart) !(Located Expr)
    deriving (Eq, Show)

-- | Simple or searched CASE expression.
data CaseExpr = CaseExpr
    { caseOperand :: !(Maybe (Located Expr))
    , caseWhens :: ![(Located Expr, Located Expr)]
    , caseElse :: !(Maybe (Located Expr))
    }
    deriving (Eq, Show)

-- | Window reference: a named window or an inline specification.
data WindowRef
    = WindowRefName !(Located NamePart)
    | WindowRefSpec !WindowSpec
    deriving (Eq, Show)

-- | Window partitioning, ordering, and frame.
data WindowSpec = WindowSpec
    { windowBaseName :: !(Maybe (Located NamePart))
    , windowPartitionBy :: ![Located Expr]
    , windowOrderBy :: ![Located OrderItem]
    , windowFrame :: !(Maybe WindowFrame)
    }
    deriving (Eq, Show)

data WindowFrame = WindowFrame
    { windowFrameUnit :: !WindowFrameUnit
    , windowFrameStart :: !WindowBound
    , windowFrameEnd :: !(Maybe WindowBound)
    }
    deriving (Eq, Show)

data WindowFrameUnit
    = FrameRows
    | FrameRange
    | FrameGroups
    deriving (Eq, Ord, Show)

data WindowBound
    = BoundUnboundedPreceding
    | BoundPreceding !(Located Expr)
    | BoundCurrentRow
    | BoundFollowing !(Located Expr)
    | BoundUnboundedFollowing
    deriving (Eq, Show)

-- | One ORDER BY item.
data OrderItem = OrderItem
    { orderExpr :: !(Located Expr)
    , orderDirection :: !(Maybe SortDirection)
    , orderNulls :: !(Maybe NullsOrder)
    }
    deriving (Eq, Show)

data SortDirection
    = SortAscending
    | SortDescending
    deriving (Eq, Ord, Show)

data NullsOrder
    = NullsFirst
    | NullsLast
    deriving (Eq, Ord, Show)

-- | Type name with arguments, suffix words, and array dimensions.
data TypeName = TypeName
    { typeNameParts :: ![Located NamePart]
    , typeNameArgs :: ![Located TypeArg]
    , typeNameSuffix :: ![Located NamePart]
    , typeNameArrayDims :: !Int
    }
    deriving (Eq, Show)

data TypeArg
    = TypeArgType !(Located TypeName)
    | TypeArgExpr !(Located Expr)
    deriving (Eq, Show)

data IndexKeyPart = IndexKeyPart
    { indexKeyExpr :: !(Located Expr)
    , indexKeyPrefixLength :: !(Maybe (Located Expr))
    , indexKeyDirection :: !(Maybe SortDirection)
    , indexKeyNulls :: !(Maybe NullsOrder)
    }
    deriving (Eq, Show)
