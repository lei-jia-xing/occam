{- | Shared internal types and block reasons used by the script-wide column
assembly passes.
-}
module Occam.Frontend.Columns.Assemble.Types (
    minBundleSites,
    blockConstraint,
    blockRead,
    blockStar,
    blockAmbiguous,
    blockUnknown,
    blockGeneration,
    blockShape,
    blockDependent,
    blockSpan,
    UnitView (..),
    UnitRefs (..),
    RelRename (..),
    FenceSet (..),
    emptyFenceSet,
    TableScope (..),
    TablePlan (..),
    Assembly (..),
) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Occam.Core.Schema qualified as Core
import Occam.Core.Semantics qualified as Core
import Occam.Frontend.Columns.Analysis (ColumnUnit, CreateColumns)

-- | A bundle needs both its definition and at least one removable use site.
minBundleSites :: Int
minBundleSites = 2

-- | A key, default, generated value, or constraint makes the column non-independent.
blockConstraint :: Text
blockConstraint = "column-constraint-ref"

-- | Another statement reads or writes the same column.
blockRead :: Text
blockRead = "column-read-elsewhere"

-- | A star projection may observe the column without naming it.
blockStar :: Text
blockStar = "column-star-projection"

-- | Duplicate or sibling relation names prevent an unambiguous binding.
blockAmbiguous :: Text
blockAmbiguous = "column-ambiguous-binding"

-- | An unknown source or opaque effect prevents proving column independence.
blockUnknown :: Text
blockUnknown = "column-unknown-source"

-- | The table's schema generation cannot be followed unambiguously.
blockGeneration :: Text
blockGeneration = "column-generation-ambiguous"

-- | The INSERT source shape cannot be mapped safely to column positions.
blockShape :: Text
blockShape = "column-insert-shape"

-- | An index, parent, or other dependent object refers to the column's table.
blockDependent :: Text
blockDependent = "column-dependent-object"

-- | A source span is absent, invalid, or not a UTF-8 boundary.
blockSpan :: Text
blockSpan = "column-span-incomplete"

-- | One parsed statement and its byte range plus facts used by assembly.
data UnitView = UnitView
    { uvIndex :: !Int
    -- ^ Zero-based position in the statement sequence.
    , uvStart :: !Int
    -- ^ Inclusive source byte offset of the statement.
    , uvEnd :: !Int
    -- ^ Exclusive source byte offset of the statement.
    , uvFacts :: !(Maybe Core.SemanticFacts)
    -- ^ Missing when semantics are unavailable.
    , uvColumn :: !ColumnUnit
    -- ^ Recovered column geometry for this statement.
    }

-- | Relation and column references gathered from one semantic statement.
data UnitRefs = UnitRefs
    { urReads :: ![Core.ReadFact]
    -- ^ Explicit source reads.
    , urWrites :: ![(Core.Name, [Core.Ident])]
    -- ^ Relation writes and named columns.
    , urTouched :: !(Set Core.Name)
    -- ^ Relations touched by ops or barriers.
    , urStars :: ![Core.NameRef]
    -- ^ Relations whose columns may be observed via @*@.
    , urUnknownSource :: !Bool
    -- ^ Whether source recovery found an unknown relation.
    , urOpaque :: !Bool
    -- ^ Whether this unit contains an opaque effect.
    }

-- | A non-temporary relation rename and its statement-order edge.
data RelRename = RelRename
    { rrUnit :: !Int
    -- ^ Zero-based statement index containing the rename.
    , rrFrom :: !Core.NameRef
    -- ^ Original relation reference and temporary flag.
    , rrTo :: !Core.Name
    -- ^ New relation name.
    }

-- | A set of named columns fenced from deletion, or a whole-object fence.
data FenceSet = FenceSet
    { fsAll :: !Bool
    -- ^ True fences every column in the scope.
    , fsTouched :: !(Set Core.Ident)
    -- ^ Individually fenced columns otherwise.
    }

-- | No columns are fenced.
emptyFenceSet :: FenceSet
emptyFenceSet = FenceSet False Set.empty

-- | A table's generation chain and the columns fenced by each ambiguity source.
data TableScope = TableScope
    { tsChain :: ![Core.Name]
    -- ^ Relation names from definition through renames.
    , tsEdgeUnits :: ![Int]
    -- ^ Statement index for each rename edge.
    , tsGeneration :: !FenceSet
    -- ^ Fences from changes across the generation chain.
    , tsAmb :: !FenceSet
    -- ^ Fences from sibling-name ambiguity.
    }

-- | All definitions of a relation encountered in source order.
data TablePlan = TablePlan
    { tpName :: !Core.Name
    -- ^ Relation name shared by these definitions.
    , tpDefines :: ![(Int, Core.TableDef, Maybe CreateColumns)]
    -- ^ Definition statement, AST, and source geometry.
    }

-- | Script-wide facts and fences accumulated by the single forward pass.
data Assembly = Assembly
    { asmViews :: ![UnitView]
    -- ^ Per-statement semantic and geometry views.
    , asmRefs :: ![Maybe UnitRefs]
    -- ^ References, preserving unavailable semantic units.
    , asmOpaqueAll :: !Bool
    -- ^ A global effect blocks all column bundles.
    , asmDependentFenced :: !(Set Core.Name)
    -- ^ Relations fenced by dependent objects.
    , asmRelationFenced :: !(Set Core.Name)
    -- ^ Relations fenced by scoped barriers.
    , asmOpaqueFenced :: !(Set Core.Name)
    -- ^ Relations touched by opaque or transactional units.
    , asmSchemaFenced :: !(Set Core.Name)
    -- ^ Schemas fenced by scoped barriers.
    , asmIndexColumns :: !(Map Core.Name [Core.Ident])
    -- ^ Explicit index columns by table.
    , asmIndexOpaque :: !(Set Core.Name)
    -- ^ Tables with opaque indexes.
    , asmParentColumns :: !(Map Core.Name (Set Core.Ident))
    -- ^ Referenced parent columns by table.
    , asmParentOpaque :: !(Set Core.Name)
    -- ^ Tables with unknown parent-column references.
    , asmRelationNames :: !(Set Core.Name)
    -- ^ Every relation name participating in DDL or reads/writes.
    , asmDdlNames :: !(Set Core.Name)
    -- ^ Relation names participating in DDL.
    , asmDefineNames :: !(Set Core.Name)
    -- ^ Relation names defined in the script.
    , asmRelRenames :: ![RelRename]
    -- ^ Relation rename edges in source order.
    , asmDrops :: !(Set Core.Name)
    -- ^ Relations dropped in the script.
    }
