{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Binary decoding for compact FFI transport from sqlparser frontend.
module Occam.Parser.Binary (
    binaryMagic,
    decodeBinaryResponse,
) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Exts (Int (..))
import Occam.Facts (
    ColumnSite (..),
    ColumnSiteKind (..),
    ColumnTarget (..),
    ParserEdit (..),
    ParserEditCategory (..),
 )
import Occam.Parser.Binary.Primitives (
    Decoder (..),
    getBool,
    getEditLabel,
    getEditReplacement,
    getIdent,
    getInt32,
    getName,
    getOptional,
    getText,
    getVec,
    getVecFixed,
    getWord32le,
    getWord8,
    withIdents,
 )
import Occam.Parser.Binary.Semantics (getSemanticFacts, getSessionInit)
import Occam.Parser.Native (NativeFacts (..), NativeResponse (..), NativeUnit (..))

-- | Magic header bytes prefixing every binary FFI response (OCCM v2).
binaryMagic :: ByteString
binaryMagic = "OCCM\x02"

-- | Decode a binary native parser response payload (without the magic header).
decodeBinaryResponse :: ByteString -> Either Text NativeResponse
decodeBinaryResponse bytes =
    let !(I# len) = BS.length bytes
     in case unDec decodeAll IntMap.empty bytes len 0# of
            (# err | #) -> Left err
            (# | (# response, pos #) #) ->
                if I# pos == BS.length bytes
                    then Right response
                    else Left "trailing unparsed bytes in binary response"
    where
        decodeAll = do
            count <- getWord32le
            identList <- getVecFixed (fromIntegral count) getText
            let !idents = IntMap.fromDistinctAscList (zip [0 ..] identList)
            withIdents idents getResponse

{-# INLINE getEditCategory #-}
getEditCategory :: Word8 -> Decoder ParserEditCategory
getEditCategory = \case
    0 -> pure EditExpression
    1 -> pure EditClause
    2 -> pure EditConstraint
    3 -> pure EditList
    4 -> pure EditColumn
    5 -> pure EditType
    6 -> pure EditLiteral
    code -> fail ("unknown parser edit category code: " ++ show code)

{-# INLINE getParserEdit #-}
getParserEdit :: Decoder ParserEdit
getParserEdit = do
    !start <- getInt32
    !end <- getInt32
    !rep <- getEditReplacement
    !rawCat <- getWord8
    !cat <- getEditCategory (rawCat .&. 0x0f)
    !label <- getEditLabel
    pure
        ParserEdit
            { parserEditStart = start
            , parserEditEnd = end
            , parserEditReplacement = rep
            , parserEditCategory = cat
            , parserEditLabel = label
            , parserEditTopProjection = (rawCat .&. 0x80) /= 0
            , parserEditStringLiteral = (rawCat .&. 0x40) /= 0
            , parserEditRecursivePredicate = (rawCat .&. 0x20) /= 0
            , parserEditLowYield = (rawCat .&. 0x10) /= 0
            }

getParserEditVec :: Decoder [ParserEdit]
getParserEditVec = getVec getParserEdit

{-# INLINE getFacts #-}
getFacts :: Decoder NativeFacts
getFacts = NativeFacts <$> getSemanticFacts <*> getParserEditVec

{-# INLINE getUnit #-}
getUnit :: Decoder NativeUnit
getUnit = do
    start <- getInt32
    end <- getInt32
    parseError <- getOptional getText
    cannotProduceResult <- getBool
    facts <- getOptional getFacts
    pure (NativeUnit start end parseError cannotProduceResult facts)

{-# INLINE getColumnSiteKind #-}
getColumnSiteKind :: Decoder ColumnSiteKind
getColumnSiteKind = do
    code <- getWord8
    case code of
        0 -> pure CreateDefSite
        1 -> pure InsertListSite
        2 -> pure ValuesItemSite
        _ -> fail ("invalid column site kind code: " ++ show code)

{-# INLINE getColumnSite #-}
getColumnSite :: Decoder ColumnSite
getColumnSite = do
    stmt <- getInt32
    start <- getInt32
    end <- getInt32
    ColumnSite stmt start end <$> getColumnSiteKind

getColumnTarget :: Decoder ColumnTarget
getColumnTarget =
    ColumnTarget
        <$> getName
        <*> getIdent
        <*> getInt32
        <*> getVec getColumnSite
        <*> getOptional getText

{-# INLINE getResponse #-}
getResponse :: Decoder NativeResponse
getResponse =
    NativeResponse
        <$> getSessionInit
        <*> getVec getUnit
        <*> getVec getColumnTarget
