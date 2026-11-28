{- | Script-wide column co-reduction bundle assembly.

Tracks table generation chains across renames, sibling-spelling ambiguity,
schema/relation/dependent fences, transaction opacity, constraint and reference
fences, positional @INSERT@/@VALUES@ binding, and site validation.
-}
module Occam.Frontend.Columns.Assemble (
    assembleColumns,
    assembleColumnsWith,
    blockConstraint,
    blockRead,
    blockStar,
    blockAmbiguous,
    blockUnknown,
    blockGeneration,
    blockShape,
    blockDependent,
    blockSpan,
) where

import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)
import Occam.Core.Schema qualified as Core
import Occam.Core.Types qualified as Core
import Occam.Facts (
    ColumnTarget (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Frontend.Columns.Analysis (
    PreParsedUnit (..),
    analyzeColumnUnitWith,
    createColumns,
 )
import Occam.Frontend.Columns.Assemble.Build (buildAssembly)
import Occam.Frontend.Columns.Assemble.Scope (
    buildScope,
    hasDuplicateNames,
    planTables,
 )
import Occam.Frontend.Columns.Assemble.Target (
    blockedTarget,
    columnTarget,
    createSites,
 )
import Occam.Frontend.Columns.Assemble.Types (
    Assembly (..),
    TablePlan (..),
    UnitView (..),
    blockAmbiguous,
    blockConstraint,
    blockDependent,
    blockGeneration,
    blockRead,
    blockShape,
    blockSpan,
    blockStar,
    blockUnknown,
 )

-- | Assemble script-wide atomic 'ColumnTarget' bundles from parsed statement units.
assembleColumns :: ByteString -> [StatementUnit] -> [ColumnTarget]
assembleColumns source units = assembleColumnsWith source [(u, Nothing) | u <- units]

-- | Assemble script-wide 'ColumnTarget' bundles reusing pre-parsed ASTs and token streams.
assembleColumnsWith :: ByteString -> [(StatementUnit, Maybe PreParsedUnit)] -> [ColumnTarget]
assembleColumnsWith source unitsWithAst =
    concatMap planTargets (planTables (asmViews assembly))
    where
        views =
            [ UnitView
                { uvIndex = idx
                , uvStart = Core.spanStartByte (Core.stmtSpan (unitStatement unit))
                , uvEnd = Core.spanEndByte (Core.stmtSpan (unitStatement unit))
                , uvFacts = fmap factSemantics (unitFacts unit)
                , uvColumn = analyzeColumnUnitWith mPreParsed source unit
                }
            | (idx, (unit, mPreParsed)) <- zip [0 :: Int ..] unitsWithAst
            ]
        assembly = buildAssembly views

        defineTargets plan mScope (unitIdx, tblDef, mGeom) =
            let cols = Core.tableColumns tblDef
                dupNames = hasDuplicateNames cols
                arity = length cols
                geometries = maybe [] createColumns mGeom
                resolveGeometry defined = \case
                    Just (name, spanForColumn) | name == Core.columnName defined -> spanForColumn
                    _ -> Nothing
                buildTarget ordinal defined geometry =
                    let createSpan = resolveGeometry defined geometry
                     in case mScope of
                            Nothing ->
                                let sites = createSites unitIdx createSpan
                                 in Just (blockedTarget (tpName plan) (Core.columnName defined) ordinal sites blockGeneration)
                            Just scope ->
                                columnTarget
                                    assembly
                                    source
                                    (tpName plan)
                                    (unitIdx, tblDef)
                                    arity
                                    dupNames
                                    scope
                                    (ordinal, defined, createSpan)
             in catMaybes (zipWith3 buildTarget [0 ..] cols (map Just geometries ++ repeat Nothing))

        planTargets plan =
            let unique = length (tpDefines plan) == 1
                mScope = case tpDefines plan of
                    [(defUnit, _, _)] | unique -> Just (buildScope assembly (tpName plan) defUnit)
                    _ -> Nothing
             in concatMap (defineTargets plan mScope) (tpDefines plan)
