{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Low-level unboxed binary decoder primitives and common interned atoms.
module Occam.Parser.Binary.Primitives (
    Decoder (..),
    getBool,
    getEditLabel,
    getEditReplacement,
    getIdent,
    getInt32,
    getInt64,
    getName,
    getNameRef,
    getOptional,
    getText,
    getVec,
    getVecFixed,
    getWord32le,
    getWord8,
    withIdents,
) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64, Word8)
import GHC.Exts (Int (..), Int#, isTrue#, (+#), (-#), (<#), (<=#))
import Occam.Core (Ident (..), Name (..), NameRef (..))

-- | Zero-allocation unboxed direct-style decoder over a strict 'ByteString' cursor.
newtype Decoder a = Decoder
    { unDec :: IntMap Text -> ByteString -> Int# -> Int# -> (# Text | (# a, Int# #) #)
    }

instance Functor Decoder where
    {-# INLINE fmap #-}
    fmap :: (a -> b) -> Decoder a -> Decoder b
    fmap f (Decoder g) = Decoder $ \idents bs len pos -> case g idents bs len pos of
        (# e | #) -> (# e | #)
        (# | (# a, pos' #) #) -> (# | (# f a, pos' #) #)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure :: a -> Decoder a
    pure a = Decoder $ \_ _ _ pos -> (# | (# a, pos #) #)

    {-# INLINE (<*>) #-}
    (<*>) :: Decoder (a -> b) -> Decoder a -> Decoder b
    Decoder gf <*> Decoder ga = Decoder $ \idents bs len pos -> case gf idents bs len pos of
        (# e | #) -> (# e | #)
        (# | (# f, pos1 #) #) -> case ga idents bs len pos1 of
            (# e | #) -> (# e | #)
            (# | (# a, pos2 #) #) -> (# | (# f a, pos2 #) #)

instance Monad Decoder where
    {-# INLINE (>>=) #-}
    (>>=) :: Decoder a -> (a -> Decoder b) -> Decoder b
    Decoder ga >>= f = Decoder $ \idents bs len pos -> case ga idents bs len pos of
        (# e | #) -> (# e | #)
        (# | (# a, pos1 #) #) -> unDec (f a) idents bs len pos1

instance MonadFail Decoder where
    {-# INLINE fail #-}
    fail :: String -> Decoder a
    fail msg = Decoder $ \_ _ _ _ -> (# T.pack msg | #)

withIdents :: IntMap Text -> Decoder a -> Decoder a
withIdents idents (Decoder f) = Decoder $ \_ bs len pos -> f idents bs len pos

getVecFixed :: Int -> Decoder a -> Decoder [a]
getVecFixed !n item = Decoder $ \idents bs len pos ->
    let go 0# p = (# | (# [], p #) #)
        go k p = case unDec item idents bs len p of
            (# err | #) -> (# err | #)
            (# | (# x, p1 #) #) -> case go (k -# 1#) p1 of
                (# err | #) -> (# err | #)
                (# | (# xs, p2 #) #) -> (# | (# x : xs, p2 #) #)
        !(I# count#) = n
     in go count# pos

{-# INLINE getWord8 #-}
getWord8 :: Decoder Word8
getWord8 = Decoder $ \_ bs len pos ->
    if isTrue# (pos <# len)
        then
            let !b = BSU.unsafeIndex bs (I# pos)
             in (# | (# b, pos +# 1# #) #)
        else (# "unexpected end of binary stream" | #)

{-# INLINE decodeWord32LE #-}
decodeWord32LE :: ByteString -> Int -> Word32
decodeWord32LE bs i =
    let !w0 = fromIntegral (BSU.unsafeIndex bs i) :: Word32
        !w1 = fromIntegral (BSU.unsafeIndex bs (i + 1)) :: Word32
        !w2 = fromIntegral (BSU.unsafeIndex bs (i + 2)) :: Word32
        !w3 = fromIntegral (BSU.unsafeIndex bs (i + 3)) :: Word32
     in w0 .|. (w1 `shiftL` 8) .|. (w2 `shiftL` 16) .|. (w3 `shiftL` 24)

{-# INLINE getWord32le #-}
getWord32le :: Decoder Word32
getWord32le = Decoder $ \_ bs len pos ->
    if isTrue# (pos +# 4# <=# len)
        then
            let !w = decodeWord32LE bs (I# pos)
             in (# | (# w, pos +# 4# #) #)
        else (# "unexpected end of binary stream" | #)

{-# INLINE getInt32 #-}
getInt32 :: Decoder Int
getInt32 = fromIntegral <$> getWord32le

{-# INLINE getBool #-}
getBool :: Decoder Bool
getBool = (/= 0) <$> getWord8

{-# INLINE decodeWord64LE #-}
decodeWord64LE :: ByteString -> Int -> Word64
decodeWord64LE bs i =
    let !w0 = fromIntegral (BSU.unsafeIndex bs i) :: Word64
        !w1 = fromIntegral (BSU.unsafeIndex bs (i + 1)) :: Word64
        !w2 = fromIntegral (BSU.unsafeIndex bs (i + 2)) :: Word64
        !w3 = fromIntegral (BSU.unsafeIndex bs (i + 3)) :: Word64
        !w4 = fromIntegral (BSU.unsafeIndex bs (i + 4)) :: Word64
        !w5 = fromIntegral (BSU.unsafeIndex bs (i + 5)) :: Word64
        !w6 = fromIntegral (BSU.unsafeIndex bs (i + 6)) :: Word64
        !w7 = fromIntegral (BSU.unsafeIndex bs (i + 7)) :: Word64
     in w0
            .|. (w1 `shiftL` 8)
            .|. (w2 `shiftL` 16)
            .|. (w3 `shiftL` 24)
            .|. (w4 `shiftL` 32)
            .|. (w5 `shiftL` 40)
            .|. (w6 `shiftL` 48)
            .|. (w7 `shiftL` 56)

{-# INLINE getInt64 #-}
getInt64 :: Decoder Integer
getInt64 = Decoder $ \_ bs len pos ->
    if isTrue# (pos +# 8# <=# len)
        then
            let !i64 = fromIntegral (decodeWord64LE bs (I# pos)) :: Int64
                !val = fromIntegral i64
             in (# | (# val, pos +# 8# #) #)
        else (# "unexpected end of binary stream" | #)

{-# INLINE getText #-}
getText :: Decoder Text
getText = getInt32 >>= getUtf8Text

{-# INLINE getUtf8Text #-}
getUtf8Text :: Int -> Decoder Text
getUtf8Text n@(I# n#) = Decoder $ \_ bs len pos ->
    decodeUtf8Slice bs len pos n n#

{-# INLINE decodeUtf8Slice #-}
decodeUtf8Slice :: ByteString -> Int# -> Int# -> Int -> Int# -> (# Text | (# Text, Int# #) #)
decodeUtf8Slice bs len pos n n#
    | n < 0 || n > I# (len -# pos) = (# "unexpected end of binary stream" | #)
    | n == 0 = (# | (# T.empty, pos #) #)
    | otherwise =
        let !slice = BSU.unsafeTake n (BSU.unsafeDrop (I# pos) bs)
         in case TE.decodeUtf8' slice of
                Left exc -> (# T.pack ("invalid UTF-8 in binary stream: " ++ show exc) | #)
                Right !txt -> (# | (# txt, pos +# n# #) #)

-- | Intern common static edit labels without UTF-8 decoding or Text allocation.
{-# INLINE getEditLabel #-}
getEditLabel :: Decoder Text
getEditLabel = do
    !code <- getWord8
    maybe getText pure (commonLabel code)

-- | Intern common edit replacements without retaining slices of the FFI buffer.
{-# INLINE getEditReplacement #-}
getEditReplacement :: Decoder ByteString
getEditReplacement = getInt32 >>= getInternedBytes

{-# INLINE getInternedBytes #-}
getInternedBytes :: Int -> Decoder ByteString
getInternedBytes n@(I# n#) = Decoder $ \_ bs len pos ->
    decodeInternedSlice bs len pos n n#

{-# INLINE decodeInternedSlice #-}
decodeInternedSlice ::
    ByteString -> Int# -> Int# -> Int -> Int# -> (# Text | (# ByteString, Int# #) #)
decodeInternedSlice bs len pos n n#
    | n < 0 || n > I# (len -# pos) = (# "unexpected end of binary stream" | #)
    | n == 0 = (# | (# BS.empty, pos #) #)
    | otherwise =
        let !rep = internReplacement bs (I# pos) n
         in (# | (# rep, pos +# n# #) #)

{-# INLINE internReplacement #-}
internReplacement :: ByteString -> Int -> Int -> ByteString
internReplacement bs off 1 = case BSU.unsafeIndex bs off of
    0x30 -> repZero
    0x31 -> repOne
    _ -> BS.copy (BSU.unsafeTake 1 (BSU.unsafeDrop off bs))
internReplacement bs off 3 =
    let !slice = BSU.unsafeTake 3 (BSU.unsafeDrop off bs)
     in if slice == repQuotedZero then repQuotedZero else BS.copy slice
internReplacement bs off 4 =
    let !slice = BSU.unsafeTake 4 (BSU.unsafeDrop off bs)
     in if slice == repNull then repNull else BS.copy slice
internReplacement bs off n =
    BS.copy (BSU.unsafeTake n (BSU.unsafeDrop off bs))

repZero, repOne, repQuotedZero, repNull :: ByteString
repZero = "0"
repOne = "1"
repQuotedZero = "'0'"
repNull = "NULL"

commonLabel :: Word8 -> Maybe Text
commonLabel = \case
    0 -> Just "remove-column_constraint"
    1 -> Just "remove-cte"
    2 -> Just "remove-cte-chunk"
    3 -> Just "remove-distinct"
    4 -> Just "remove-distinct_on_expression"
    5 -> Just "remove-fetch"
    6 -> Just "remove-from-relation"
    7 -> Just "remove-from-relation-chunk"
    8 -> Just "remove-function_argument"
    9 -> Just "remove-function_argument-chunk"
    10 -> Just "remove-group_by"
    11 -> Just "remove-group_by_expression"
    12 -> Just "remove-having"
    13 -> Just "remove-in_list"
    14 -> Just "remove-in_list-chunk"
    15 -> Just "remove-join"
    16 -> Just "remove-limit"
    17 -> Just "remove-list-chunk"
    18 -> Just "remove-offset"
    19 -> Just "remove-order_by"
    20 -> Just "remove-order_by_expression"
    21 -> Just "remove-returning"
    22 -> Just "remove-returning_expression"
    23 -> Just "remove-select_expression"
    24 -> Just "remove-select_expression-chunk"
    25 -> Just "remove-table_constraint"
    26 -> Just "remove-tuple_item"
    27 -> Just "remove-tuple_item-chunk"
    28 -> Just "remove-update_assignment"
    29 -> Just "remove-values_row"
    30 -> Just "remove-values_row-chunk"
    31 -> Just "remove-where"
    32 -> Just "remove-window_frame"
    33 -> Just "remove-window_order_by"
    34 -> Just "remove-window_order_by_expression"
    35 -> Just "remove-window_partition_by"
    36 -> Just "remove-window_partition_by_expression"
    37 -> Just "remove-with"
    38 -> Just "simplify-between"
    39 -> Just "simplify-binary_expression"
    40 -> Just "simplify-case_else"
    41 -> Just "simplify-case_literal"
    42 -> Just "simplify-case_result"
    43 -> Just "simplify-cast"
    44 -> Just "simplify-cast_literal"
    45 -> Just "simplify-data_type"
    46 -> Just "simplify-exists"
    47 -> Just "simplify-function_literal"
    48 -> Just "simplify-function_to_argument"
    49 -> Just "simplify-in_list"
    50 -> Just "simplify-in_subquery"
    51 -> Just "simplify-is_null"
    52 -> Just "simplify-like"
    53 -> Just "simplify-literal"
    54 -> Just "simplify-parenthesized_expression"
    55 -> Just "simplify-set_operation_left"
    56 -> Just "simplify-set_operation_right"
    57 -> Just "simplify-unary_expression"
    58 -> Just "simplify-window_spec_empty"
    59 -> Just "remove-column_create_def"
    60 -> Just "remove-column_insert_item"
    61 -> Just "remove-column_values_item"
    62 -> Just "drop-column"
    _ -> Nothing

{-# INLINE getVec #-}
getVec :: Decoder a -> Decoder [a]
getVec (Decoder item) = Decoder $ \idents bs len pos -> case unDec getInt32 idents bs len pos of
    (# err | #) -> (# err | #)
    (# | (# I# count, pos0 #) #) ->
        if isTrue# (count <# 0#) || isTrue# (len -# pos0 <# count)
            then (# "invalid vector length in binary stream" | #)
            else go count pos0
        where
            go 0# p = (# | (# [], p #) #)
            go k p = case item idents bs len p of
                (# err | #) -> (# err | #)
                (# | (# x, p1 #) #) -> case go (k -# 1#) p1 of
                    (# err | #) -> (# err | #)
                    (# | (# xs, p2 #) #) -> (# | (# x : xs, p2 #) #)

{-# INLINE getIdent #-}
getIdent :: Decoder Ident
getIdent = do
    idx <- getWord32le
    Decoder $ \idents _ _ pos ->
        case IntMap.lookup (fromIntegral idx) idents of
            Just txt -> (# | (# Ident txt, pos #) #)
            Nothing -> (# "invalid interned identifier index" | #)

{-# INLINE getName #-}
getName :: Decoder Name
getName = Name <$> getVec getIdent

{-# INLINE getNameRef #-}
getNameRef :: Decoder NameRef
getNameRef = NameRef <$> getName <*> getBool

{-# INLINE getOptional #-}
getOptional :: Decoder a -> Decoder (Maybe a)
getOptional getVal = do
    tag <- getWord8
    case tag of
        0 -> pure Nothing
        1 -> Just <$> getVal
        _ -> fail ("invalid Maybe tag: " ++ show tag)
