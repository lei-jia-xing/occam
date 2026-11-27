-- | Half-open UTF-8 byte spans, located values, and boundary validation.
module Occam.Frontend.Span (
    ByteSpan (..),
    Located (..),
    spanLength,
    spanIsEmpty,
    mergeSpan,
    toCoreSpan,
    fromCoreSpan,
    isUtf8Boundary,
    isValidSpan,
    isNonEmptyValidSpan,
    sliceSpan,
    unsafeSliceSpan,
    validateUtf8,
) where

import Control.Monad (guard)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Data (Data)
import Occam.Core (Span (..))

-- | Half-open 0-based UTF-8 byte range @[spanStart, spanEnd)@.
data ByteSpan = ByteSpan
    { spanStart :: !Int
    , spanEnd :: !Int
    }
    deriving (Eq, Ord, Show, Data)

instance Semigroup ByteSpan where
    ByteSpan s1 e1 <> ByteSpan s2 e2 = ByteSpan (min s1 s2) (max e1 e2)

-- | A syntax or token value annotated with its exact source 'ByteSpan'.
data Located a = L
    { locSpan :: !ByteSpan
    , locValue :: !a
    }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Data)

-- | Number of bytes covered by a 'ByteSpan' (clamped at zero).
{-# INLINE spanLength #-}
spanLength :: ByteSpan -> Int
spanLength (ByteSpan s e) = max 0 (e - s)

-- | True when @spanStart >= spanEnd@.
{-# INLINE spanIsEmpty #-}
spanIsEmpty :: ByteSpan -> Bool
spanIsEmpty (ByteSpan s e) = s >= e

-- | Convex hull merge of two 'ByteSpan's.
{-# INLINE mergeSpan #-}
mergeSpan :: ByteSpan -> ByteSpan -> ByteSpan
mergeSpan = (<>)

-- | Convert a frontend 'ByteSpan' to 'Occam.Core.Span'.
{-# INLINE toCoreSpan #-}
toCoreSpan :: ByteSpan -> Span
toCoreSpan (ByteSpan s e) = Span s e

-- | Convert an 'Occam.Core.Span' to a frontend 'ByteSpan'.
{-# INLINE fromCoreSpan #-}
fromCoreSpan :: Span -> ByteSpan
fromCoreSpan (Span s e) = ByteSpan s e

{- | True when @offset@ is in @[0, BS.length bs]@ and does not point to a UTF-8
continuation byte (@10xxxxxx@).
-}
{-# INLINE isUtf8Boundary #-}
isUtf8Boundary :: ByteString -> Int -> Bool
isUtf8Boundary bs offset =
    offset >= 0
        && offset <= len
        && (offset == 0 || offset == len || (BSU.unsafeIndex bs offset .&. 0xc0) /= 0x80)
    where
        !len = BS.length bs

-- | True when @spanStart <= spanEnd <= BS.length bs@ on UTF-8 boundaries.
{-# INLINE isValidSpan #-}
isValidSpan :: ByteString -> ByteSpan -> Bool
isValidSpan bs (ByteSpan s e) =
    s >= 0
        && s <= e
        && e <= BS.length bs
        && isUtf8Boundary bs s
        && isUtf8Boundary bs e

-- | True when 'isValidSpan' holds and the span is strictly non-empty.
{-# INLINE isNonEmptyValidSpan #-}
isNonEmptyValidSpan :: ByteString -> ByteSpan -> Bool
isNonEmptyValidSpan bs sp@(ByteSpan s e) = s < e && isValidSpan bs sp

-- | Checked slice of @bs@ at @sp@, returning 'Nothing' if out of bounds or mid-codepoint.
sliceSpan :: ByteSpan -> ByteString -> Maybe ByteString
sliceSpan sp bs = unsafeSliceSpan sp bs <$ guard (isValidSpan bs sp)

-- | Zero-copy slice of @bs@ at @sp@. Caller must ensure @0 <= spanStart <= spanEnd <= BS.length bs@.
{-# INLINE unsafeSliceSpan #-}
unsafeSliceSpan :: ByteSpan -> ByteString -> ByteString
unsafeSliceSpan (ByteSpan s e) bs = BSU.unsafeTake (e - s) (BSU.unsafeDrop s bs)

{- | Validate RFC 3629 UTF-8 over @bs@, returning @Left invalidStartOffset@
pointing to the start byte of the first invalid UTF-8 sequence.
-}
validateUtf8 :: ByteString -> Either Int ()
validateUtf8 bs = go 0
    where
        !len = BS.length bs
        at = BSU.unsafeIndex bs

        {-# INLINE isCont #-}
        isCont b = (b .&. 0xc0) == 0x80

        {-# INLINE inRange #-}
        inRange lo hi b = b >= lo && b <= hi

        go !i
            | i >= len = Right ()
            | otherwise = step (at i)
            where
                step !b0
                    | b0 < 0x80 = go (i + 1)
                    | inRange 0xc2 0xdf b0 = need 2 (isCont (at (i + 1)))
                    | b0 == 0xe0 = need 3 (inRange 0xa0 0xbf (at (i + 1)) && isCont (at (i + 2)))
                    | inRange 0xe1 0xec b0 || b0 == 0xee || b0 == 0xef =
                        need 3 (isCont (at (i + 1)) && isCont (at (i + 2)))
                    | b0 == 0xed = need 3 (inRange 0x80 0x9f (at (i + 1)) && isCont (at (i + 2)))
                    | b0 == 0xf0 =
                        need 4 (inRange 0x90 0xbf (at (i + 1)) && isCont (at (i + 2)) && isCont (at (i + 3)))
                    | inRange 0xf1 0xf3 b0 =
                        need 4 (isCont (at (i + 1)) && isCont (at (i + 2)) && isCont (at (i + 3)))
                    | b0 == 0xf4 =
                        need 4 (inRange 0x80 0x8f (at (i + 1)) && isCont (at (i + 2)) && isCont (at (i + 3)))
                    | otherwise = Left i
                {-# INLINE need #-}
                need !width cond
                    | i + width <= len && cond = go (i + width)
                    | otherwise = Left i
