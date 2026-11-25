-- | Static missing-definition and transaction-opener requirements for early rejection.
module Occam.Certificate.MissingDef (
    buildMissingDefReqs,
) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Occam.Core (
    BarrierReason (..),
    BarrierScope (..),
    CatalogOp (..),
    DataWrite (..),
    Ident (..),
    Name (..),
    NameRef (..),
    ReadFact (..),
    ReadMode (..),
    RenameOp (..),
    ScopedBarrier (..),
    SemanticFacts (..),
    SequenceDef (..),
    Statement (..),
    StatementId (..),
    TableDef (..),
    TxEffect (..),
    ViewDef (..),
 )
import Occam.Facts (StatementFacts (..), StatementUnit (..))

data MissingDefAcc = MissingDefAcc
    { mdaRelDefs :: !(Map Text IntSet)
    , mdaTxOpens :: !IntSet
    , mdaSpOpens :: !(Map Text IntSet)
    , mdaOpaqueDdl :: !IntSet
    , mdaOpaqueTx :: !IntSet
    , mdaReqs :: !(IntMap [IntSet])
    }

-- | Precompute per-statement required prior definers/openers in source order.
buildMissingDefReqs :: [StatementUnit] -> IntMap [IntSet]
buildMissingDefReqs =
    mdaReqs . foldl' stepMissingDefUnit emptyAcc
    where
        emptyAcc = MissingDefAcc Map.empty IntSet.empty Map.empty IntSet.empty IntSet.empty IntMap.empty

stepMissingDefUnit :: MissingDefAcc -> StatementUnit -> MissingDefAcc
stepMissingDefUnit acc unit = case unitFacts unit of
    Nothing ->
        acc
            { mdaOpaqueDdl = IntSet.insert sid (mdaOpaqueDdl acc)
            , mdaOpaqueTx = IntSet.insert sid (mdaOpaqueTx acc)
            }
    Just facts -> stepMissingDefFacts sid acc facts
    where
        !sid = unStatementId (stmtId (unitStatement unit))

stepMissingDefFacts :: Int -> MissingDefAcc -> StatementFacts -> MissingDefAcc
stepMissingDefFacts sid acc facts =
    MissingDefAcc
        { mdaRelDefs = relDefs'
        , mdaTxOpens = txOpens'
        , mdaSpOpens = spOpens'
        , mdaOpaqueDdl =
            if any isOpaqueDdlBarrier barriers then IntSet.insert sid (mdaOpaqueDdl acc) else mdaOpaqueDdl acc
        , mdaOpaqueTx =
            if any isOpaqueTxBarrier barriers then IntSet.insert sid (mdaOpaqueTx acc) else mdaOpaqueTx acc
        , mdaReqs = if null allReqs then mdaReqs acc else IntMap.insert sid allReqs (mdaReqs acc)
        }
    where
        sem = factSemantics facts
        ops = factOps sem
        barriers = factBarriers sem
        txs = factPreTx sem ++ factPostTx sem
        -- Never require referenced relations inside deferred view/trigger DDL or DROP.
        skipRelReqs = any isDeferredOrDropOp ops
        selfDefined = Set.fromList (concatMap opDefinedRelNames ops)
        refRels
            | skipRelReqs = []
            | otherwise =
                Set.toList (Set.fromList (concatMap opReferencedRelNames ops) `Set.difference` selfDefined)
        relReqs =
            [ IntSet.union defs (mdaOpaqueDdl acc)
            | rel <- refRels
            , Just defs <- [Map.lookup rel (mdaRelDefs acc)]
            ]
        txReqs =
            [ req
            | tx <- txs
            , Just req <- [txRequirement tx (mdaTxOpens acc) (mdaSpOpens acc) (mdaOpaqueTx acc)]
            ]
        allReqs = dedupIntSetsSmall (relReqs ++ txReqs)
        relDefs' =
            foldl'
                (\m rel -> Map.insertWith IntSet.union rel (IntSet.singleton sid) m)
                (mdaRelDefs acc)
                (Set.toList selfDefined)
        (txOpens', spOpens') = foldl' (recordTxOpener sid) (mdaTxOpens acc, mdaSpOpens acc) txs

isDeferredOrDropOp :: CatalogOp -> Bool
isDeferredOrDropOp = \case
    DefineTableOp _ -> True
    DefineViewOp _ -> True
    DefineTriggerOp _ -> True
    DefineIndexOp _ -> True
    DropCatalogOp _ -> True
    AlterTableOp _ -> True
    _ -> False

baseRelName :: Name -> Text
baseRelName (Name []) = T.empty
baseRelName (Name parts) = T.toLower (identText (lastPart parts))
    where
        lastPart [x] = x
        lastPart (_ : xs) = lastPart xs
        lastPart [] = Ident T.empty

opDefinedRelNames :: CatalogOp -> [Text]
opDefinedRelNames = \case
    DefineTableOp td -> [baseRelName (tableName td)]
    DefineViewOp vd -> [baseRelName (viewName vd)]
    DefineSequenceOp sd -> [baseRelName (sequenceName sd)]
    RenameOp (RenameRelation _ newName _) -> [baseRelName newName]
    _ -> []

opReferencedRelNames :: CatalogOp -> [Text]
opReferencedRelNames = \case
    ReadOp rf
        | readMode rf /= ReadSchema -> [baseRelName (nameRefName (readSource rf))]
    WriteDataOp dw ->
        baseRelName (nameRefName (dataWriteTarget dw))
            : [baseRelName (nameRefName (readSource rf)) | rf <- dataWriteReads dw, readMode rf /= ReadSchema]
    _ -> []

txRequirement :: TxEffect -> IntSet -> Map Text IntSet -> IntSet -> Maybe IntSet
txRequirement tx txOpens spOpens opaqueTx = case tx of
    TxCommit _
        | not (IntSet.null txOpens) -> Just (IntSet.union txOpens opaqueTx)
    TxRollbackAll
        | not (IntSet.null txOpens) -> Just (IntSet.union txOpens opaqueTx)
    TxRollbackTo sp ->
        IntSet.union opaqueTx <$> Map.lookup (T.toLower (identText sp)) spOpens
    TxReleaseSavepoint sp ->
        IntSet.union opaqueTx <$> Map.lookup (T.toLower (identText sp)) spOpens
    _ -> Nothing

recordTxOpener :: Int -> (IntSet, Map Text IntSet) -> TxEffect -> (IntSet, Map Text IntSet)
recordTxOpener sid (txOpens, spOpens) = \case
    TxBegin _ -> (IntSet.insert sid txOpens, spOpens)
    TxSavepoint sp ->
        let spKey = T.toLower (identText sp)
         in (IntSet.insert sid txOpens, Map.insertWith IntSet.union spKey (IntSet.singleton sid) spOpens)
    _ -> (txOpens, spOpens)

isOpaqueDdlBarrier :: ScopedBarrier -> Bool
isOpaqueDdlBarrier sb =
    barrierScope sb == GlobalScope
        || case barrierReason sb of
            SyntaxErrorBarrier -> True
            UnknownStatementBarrier -> True
            UnknownRelationBarrier -> True
            UnknownDDLBarrier -> True
            UnknownDMLBarrier -> True
            UnknownAlterBarrier -> True
            UnknownViewBarrier -> True
            UnknownSessionEffectBarrier -> True
            UnsupportedTriggerBodyBarrier -> True
            OpaqueTriggerBodyBarrier -> True
            _ -> False

isOpaqueTxBarrier :: ScopedBarrier -> Bool
isOpaqueTxBarrier sb = case barrierReason sb of
    SyntaxErrorBarrier -> True
    UnknownStatementBarrier -> True
    UnknownSessionEffectBarrier -> True
    UnsupportedTransactionBarrier -> True
    _ -> False

dedupIntSetsSmall :: [IntSet] -> [IntSet]
dedupIntSetsSmall [] = []
dedupIntSetsSmall (x : xs) = x : dedupIntSetsSmall (filter (/= x) xs)
