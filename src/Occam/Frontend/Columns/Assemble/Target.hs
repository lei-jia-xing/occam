-- | Validation and construction of atomic column deletion bundles.
module Occam.Frontend.Columns.Assemble.Target (
    createSites,
    blockedTarget,
    columnTarget,
) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Foldable qualified as Foldable
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V
import Occam.Core.Schema qualified as Core
import Occam.Facts (
    ColumnSite (..),
    ColumnSiteKind (..),
    ColumnTarget (..),
 )
import Occam.Frontend.Columns.Analysis (
    ColumnUnit (..),
    InsertColumns (..),
    InsertList (..),
    InsertShape (..),
 )
import Occam.Frontend.Columns.Assemble.Scope (activeGenerationName, tableInSchema)
import Occam.Frontend.Columns.Assemble.Types (
    Assembly (..),
    FenceSet (..),
    TableScope (..),
    UnitRefs (..),
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
    minBundleSites,
 )
import Occam.Frontend.Span (isUtf8Boundary)

-- | Build a one-based CREATE-definition site when source geometry is known.
createSites :: Int -> Maybe (Int, Int) -> [ColumnSite]
createSites unitIdx = \case
    Just (s, e) -> [ColumnSite (unitIdx + 1) s e CreateDefSite]
    Nothing -> []

validateSites :: [UnitView] -> ByteString -> [ColumnSite] -> Maybe [ColumnSite]
validateSites views source rawSites =
    let sorted = sortOn (\s -> (siteStmt s, siteStart s)) rawSites
     in if checkSites Nothing sorted then Just sorted else Nothing
    where
        checkSites _ [] = True
        checkSites prev (s : rest)
            | siteStart s >= siteEnd s = False
            | siteStmt s < 1 = False
            | otherwise = case drop (siteStmt s - 1) views of
                (v : _)
                    | siteStart s >= uvStart v
                    , siteEnd s <= uvEnd v
                    , isUtf8Boundary source (siteStart s)
                    , isUtf8Boundary source (siteEnd s)
                    , maybe True (\(pStmt, pEnd) -> pStmt /= siteStmt s || pEnd <= siteStart s) prev ->
                        checkSites (Just (siteStmt s, siteEnd s)) rest
                _ -> False

referenceFence :: Assembly -> [Core.Name] -> Core.Ident -> Maybe Text
referenceFence asm chain col = Foldable.foldl' (<|>) Nothing (map checkRefs (catMaybes (asmRefs asm)))
    where
        inChain n = n `elem` chain
        checkRefs refs
            | any (inChain . Core.nameRefName) (urStars refs) = Just blockStar
            | any
                (\r -> inChain (Core.nameRefName (Core.readSource r)) && col `elem` Core.readColumns r)
                (urReads refs) =
                Just blockRead
            | any (\(t, cols) -> inChain t && col `elem` cols) (urWrites refs) =
                Just blockRead
            | urUnknownSource refs && any inChain (Set.toList (urTouched refs)) =
                Just blockUnknown
            | urOpaque refs && any inChain (Set.toList (urTouched refs)) =
                Just blockUnknown
            | otherwise = Nothing
constraintTouches :: Core.Constraint -> Core.Ident -> Bool
constraintTouches c col = case c of
    Core.PrimaryKeyConstraint _ cols -> col `elem` cols
    Core.UniqueConstraint _ cols -> col `elem` cols
    Core.CheckConstraint _ cols _ -> col `elem` cols
    Core.IndexConstraint _ cols _ -> col `elem` cols
    Core.ForeignKeyConstraint fk -> col `elem` Core.foreignKeyChildColumns fk
    Core.UsingIndexConstraint {} -> False
    Core.OtherConstraint _ -> False

constraintIsOpaque :: Core.Constraint -> Bool
constraintIsOpaque constraint = case constraint of
    Core.UsingIndexConstraint {} -> True
    Core.OtherConstraint _ -> True
    _ -> False

insertContribution ::
    InsertColumns ->
    Core.Ident ->
    Int ->
    Int ->
    Int ->
    Either Text [ColumnSite]
insertContribution ins col ordinal createArity stmtId = case insertShape ins of
    ShapeExotic -> Left blockShape
    ShapeNeutral -> Right []
    ShapeSelectSource -> case insertExplicit ins of
        Just list | Map.member col (insertListColumns list) -> Left blockShape
        Nothing -> Left blockShape
        _ -> Right []
    ShapePlainValues
        | null (insertRows ins) -> Left blockSpan
        | otherwise -> case insertExplicit ins of
            Just list -> case Map.lookup col (insertListColumns list) of
                Nothing -> Right []
                Just Nothing -> Left blockAmbiguous
                Just (Just (pos, Just (s, e)))
                    | insertRowWidth ins == Just (insertListWidth list) ->
                        (ColumnSite stmtId s e InsertListSite :) <$> traverse (rowSiteAt pos) (insertRows ins)
                Just (Just _) -> Left blockSpan
            Nothing
                | insertRowWidth ins /= Just createArity -> Left blockSpan
                | otherwise -> traverse (rowSiteAt ordinal) (insertRows ins)
    where
        rowSiteAt idx row = case row V.!? idx of
            Just (Just (s, e)) -> Right (ColumnSite stmtId s e ValuesItemSite)
            _ -> Left blockSpan

-- | Construct a column target blocked by a specific safety reason.
blockedTarget :: Core.Name -> Core.Ident -> Int -> [ColumnSite] -> Text -> ColumnTarget
blockedTarget tbl col ordinal sites cause =
    ColumnTarget
        { targetTable = tbl
        , targetColumn = col
        , targetOrdinal = ordinal
        , targetSites = sites
        , targetBlocked = Just cause
        }

-- | Evaluate static fences, references, and INSERT positions for one column.
columnTarget ::
    Assembly ->
    ByteString ->
    Core.Name ->
    (Int, Core.TableDef) ->
    Int ->
    Bool ->
    TableScope ->
    (Int, Core.ColumnDef, Maybe (Int, Int)) ->
    Maybe ColumnTarget
columnTarget asm source tbl (unitIdx, tblDef) createArity createDupNames scope (ordinal, defined, createSpan)
    | asmOpaqueAll asm = Just (blockedTarget tbl col ordinal initSites blockUnknown)
    | isNothing blockedAfterInserts =
        case validateSites (asmViews asm) source finalSites of
            Nothing -> Just (blockedTarget tbl col ordinal finalSites blockSpan)
            Just validSites
                | length validSites < minBundleSites -> Nothing
                | otherwise ->
                    Just
                        ColumnTarget
                            { targetTable = tbl
                            , targetColumn = col
                            , targetOrdinal = ordinal
                            , targetSites = validSites
                            , targetBlocked = Nothing
                            }
    | otherwise =
        Just
            ColumnTarget
                { targetTable = tbl
                , targetColumn = col
                , targetOrdinal = ordinal
                , targetSites = finalSites
                , targetBlocked = blockedAfterInserts
                }
    where
        col = Core.columnName defined
        initSites = createSites unitIdx createSpan

        blockedStatic =
            Foldable.foldl'
                (<|>)
                Nothing
                [ blockSpan <$ guardMaybe (isNothing createSpan)
                , blockAmbiguous <$ guardMaybe createDupNames
                , blockGeneration
                    <$ guardMaybe
                        ( isJust (Core.tableLike tblDef)
                            || not (null (Core.tableSources tblDef))
                            || isJust (Core.tableOnCommit tblDef)
                            || Core.tableTemporary tblDef
                        )
                , blockGeneration
                    <$ guardMaybe (fsAll (tsGeneration scope) || Set.member col (fsTouched (tsGeneration scope)))
                , blockAmbiguous <$ guardMaybe (fsAll (tsAmb scope) || Set.member col (fsTouched (tsAmb scope)))
                , blockUnknown <$ guardMaybe (any (`Set.member` asmRelationFenced asm) (tsChain scope))
                , blockUnknown <$ guardMaybe (any (`Set.member` asmOpaqueFenced asm) (tsChain scope))
                , blockUnknown
                    <$ guardMaybe
                        (any (\s -> any (`tableInSchema` s) (tsChain scope)) (Set.toList (asmSchemaFenced asm)))
                , blockDependent <$ guardMaybe (any (`Set.member` asmDependentFenced asm) (tsChain scope))
                , blockDependent <$ guardMaybe (any (`Set.member` asmIndexOpaque asm) (tsChain scope))
                , blockConstraint <$ guardMaybe (any constraintIsOpaque (Core.tableConstraints tblDef))
                , blockConstraint
                    <$ guardMaybe
                        ( Core.columnHasDefault defined
                            || Core.columnGenerated defined
                            || not (null (Core.columnReferences defined))
                            || any (`constraintTouches` col) (Core.tableConstraints tblDef)
                        )
                , blockDependent
                    <$ guardMaybe
                        (any (maybe False (col `elem`) . (`Map.lookup` asmIndexColumns asm)) (tsChain scope))
                , blockDependent <$ guardMaybe (any (`Set.member` asmParentOpaque asm) (tsChain scope))
                , blockDependent
                    <$ guardMaybe
                        (any (maybe False (Set.member col) . (`Map.lookup` asmParentColumns asm)) (tsChain scope))
                ]

        blockedWithRefs = blockedStatic <|> referenceFence asm (tsChain scope) col

        (finalSites, blockedAfterInserts) = case blockedWithRefs of
            Just cause -> (initSites, Just cause)
            Nothing -> collectInserts initSites (asmViews asm)

        collectInserts accSites [] = (accSites, Nothing)
        collectInserts accSites (v : vs) = case uvFacts v of
            Nothing -> collectInserts accSites vs
            Just _ -> stepViewInserts accSites (cuInserts (uvColumn v)) vs
            where
                activeName = activeGenerationName scope (uvIndex v)
                stepViewInserts acc [] rest = collectInserts acc rest
                stepViewInserts acc (ins : inss) rest = case insertTarget ins of
                    Just tgt
                        | Core.nameRefName tgt == activeName ->
                            if uvIndex v < unitIdx
                                then (acc, Just blockGeneration)
                                else case insertContribution ins col ordinal createArity (uvIndex v + 1) of
                                    Left cause -> (acc, Just cause)
                                    Right newSites -> stepViewInserts (acc ++ newSites) inss rest
                    _ -> stepViewInserts acc inss rest

        guardMaybe True = Just ()
        guardMaybe False = Nothing
