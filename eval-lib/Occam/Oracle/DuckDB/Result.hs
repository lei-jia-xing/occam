{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | DuckDB column-type support checks and typed cell\/chunk observation helpers.
module Occam.Oracle.DuckDB.Result (
    DuckDBColumnType (..),
    contractTypeTag,
    convertCell,
    encodeExpectedRows,
    matchResult,
    matchResultWithHeader,
    observeRows,
    readColumnType,
    readResultColumns,
    supportedType,
) where

import Control.Exception.Safe (bracket)
import Control.Monad (forM, guard, when, zipWithM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Data.Aeson (
    FromJSON (..),
    Result (Error, Success),
    Value (..),
    fromJSON,
    object,
    (.=),
 )
import Data.Aeson.KeyMap qualified as KM
import Data.Bits (shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable, peek, peekByteOff, peekElemOff, poke)
import GHC.Float (castDoubleToWord64)
import Occam.Oracle.DuckDB.FFI (
    DuckDBAPI (..),
    DuckDBDecimalAPI (..),
    DuckDBError (..),
    DuckDBResult,
    IdxT,
    fetchChunk,
    matchRows,
    mkChunkSize,
    mkChunkVector,
    mkDestroyChunk,
    mkValidity,
    mkVectorData,
    vectorBytes,
 )
import Occam.Oracle.Result (ResultSet (..))

-- | Supported DuckDB result column storage representation.
data DuckDBColumnType
    = ScalarColumn !CInt
    | DecimalColumn !Word8 !Word8 !CInt

-- | Pack a column type into a compact contract tag; decimal columns pack width and scale alongside base tag @19@.
contractTypeTag :: DuckDBColumnType -> Int
contractTypeTag (ScalarColumn ty) = fromIntegral ty
contractTypeTag (DecimalColumn width scale _) =
    19 + (fromIntegral width `shiftL` 8) + (fromIntegral scale `shiftL` 16)

-- | Inspect a DuckDB column type and metadata, returning 'Nothing' if unsupported for result comparison.
readColumnType :: DuckDBAPI -> Ptr DuckDBResult -> (CInt, IdxT) -> IO (Maybe DuckDBColumnType)
readColumnType api result (ty, column)
    | supportedType ty = pure (Just (ScalarColumn ty))
    | ty == 19
    , Just DuckDBDecimalAPI {..} <- apiDecimalAPI api = alloca $ \logicalPtr ->
        bracket
            (do logical <- apiColumnLogicalType result column; poke logicalPtr logical; pure logical)
            (const (apiDestroyLogicalType logicalPtr))
            $ \logical ->
                if logical == nullPtr
                    then pure Nothing
                    else do
                        width <- apiDecimalWidth logical
                        scale <- apiDecimalScale logical
                        storage <- apiDecimalStorage logical
                        pure $ do
                            guard (width > 0 && scale <= width && storage `elem` [3, 4, 5, 16])
                            pure (DecimalColumn width scale storage)
    | otherwise = pure Nothing

-- | Read a typed value from a DuckDB chunk vector (preserving full strings with embedded NULs).
convertCell :: DuckDBAPI -> (DuckDBColumnType, Ptr (), Ptr Word64) -> IdxT -> IO Value
convertCell api (colType, values, validity) row = do
    valid <- if validity == nullPtr then pure 1 else apiRowValid api validity row
    if valid == 0
        then pure Null
        else case colType of
            DecimalColumn _ scale storage -> do
                unscaled <- readDecimalStorage storage
                pure (Number (fromRational (unscaled % (10 ^ scale))))
            ScalarColumn ty -> readScalar ty
    where
        readDecimalStorage storage = case storage of
            3 -> toInteger <$> item @Int16
            4 -> toInteger <$> item @Int32
            5 -> toInteger <$> item @Int64
            _ -> wide signedHigh

        readScalar ty = case ty of
            1 -> Bool . (/= (0 :: Word8)) <$> item
            2 -> integral @Int8
            3 -> integral @Int16
            4 -> integral @Int32
            5 -> integral @Int64
            6 -> integral @Word8
            7 -> integral @Word16
            8 -> integral @Word32
            9 -> integral @Word64
            10 -> floatBits . realToFrac <$> item @Float
            11 -> floatBits <$> item @Double
            16 -> Number . fromInteger <$> wide signedHigh
            17 -> do
                value <- readBytes
                either (fail . show) (pure . String) (TE.decodeUtf8' value)
            18 -> do
                value <- readBytes
                pure (object ["blob" .= BS.unpack value])
            32 -> Number . fromInteger <$> wide toInteger
            36 -> pure Null
            _ -> fail ("Unsupported DuckDB result type " ++ show ty)
        item :: (Storable a) => IO a
        item = peekElemOff (castPtr values) (fromIntegral row)
        integral :: forall a. (Integral a, Storable a) => IO Value
        integral = Number . fromIntegral <$> item @a
        floatBits value = object ["float64_bits" .= castDoubleToWord64 value]
        signedHigh = toInteger . (fromIntegral :: Word64 -> Int64)
        wide highInteger = do
            let offset = fromIntegral row * 16
            low <- peekByteOff values offset :: IO Word64
            high <- peekByteOff values (offset + 8) :: IO Word64
            pure ((highInteger high `shiftL` 64) + toInteger low)
        readBytes = evalContT do
            sizePtr <- ContT alloca
            liftIO do
                ptr <- vectorBytes (apiStringData api) values row sizePtr
                size <- peek sizePtr
                if size == 0 then pure BS.empty else BS.packCStringLen (ptr, fromIntegral size)

-- | Read column names and native column type IDs from an executed @duckdb_result@.
readResultColumns :: DuckDBAPI -> Ptr DuckDBResult -> IO ([Text], [CInt])
readResultColumns api ptr = do
    count <- apiColumnCount api ptr
    let columns = takeWhile (< count) [0 ..]
    names <- forM columns $ \col -> do
        name <- apiColumnName api ptr col
        if name == nullPtr
            then fail "DuckDB omitted a result column name"
            else TE.decodeUtf8 <$> BS.packCString name
    types <- mapM (apiColumnType api ptr) columns
    pure (names, types)

-- | Return 'True' for scalar DuckDB column type IDs supported by C-side binary encoding.
supportedType :: (Integral a) => a -> Bool
supportedType ty = (ty >= 1 && ty <= 11) || ty `elem` [16, 17, 18, 32, 36]

-- | Extract and decode all rows and column metadata from a native @duckdb_result@.
observeRows :: DuckDBAPI -> Ptr DuckDBResult -> IO (Either DuckDBError ResultSet)
observeRows api ptr = do
    (names, types) <- readResultColumns api ptr
    descriptors <- traverse (readColumnType api ptr) (zip types [0 ..])
    case sequence descriptors of
        Nothing -> pure (Left (DuckDBUnsupportedResultTypes (map fromIntegral types)))
        Just columnTypes -> do
            rows <- readChunks (zip columnTypes [0 ..]) []
            pure (Right (ResultSet names (map contractTypeTag columnTypes) rows))
    where
        readChunks columnTypes accumulated = do
            chunkRows <- evalContT do
                chunkPtr <- ContT alloca
                chunk <-
                    ContT $
                        bracket
                            (do chunk <- fetchChunk (apiFetchChunk api) ptr; poke chunkPtr chunk; pure chunk)
                            (\c -> when (c /= nullPtr) (mkDestroyChunk (apiDestroyChunk api) chunkPtr))
                if chunk == nullPtr
                    then pure Nothing
                    else liftIO do
                        size <- mkChunkSize (apiChunkSize api) chunk
                        vectors <- forM columnTypes $ \(ty, col) -> do
                            vector <- mkChunkVector (apiChunkVector api) chunk col
                            values <- mkVectorData (apiVectorData api) vector
                            validity <- mkValidity (apiValidity api) vector
                            pure (ty, values, validity)
                        Just
                            <$> forM (takeWhile (< size) [0 ..]) (\row -> mapM (\vector -> convertCell api vector row) vectors)
            case chunkRows of
                Nothing -> pure (concat (reverse accumulated))
                Just rows -> readChunks columnTypes (rows : accumulated)

-- | Compare a @duckdb_result@ header and encoded rows directly in C without materializing Haskell values.
matchResult ::
    [CInt] -> [Text] -> ByteString -> DuckDBAPI -> Ptr DuckDBResult -> IO (Either DuckDBError Bool)
matchResult types names expected api ptr = do
    (actualNames, actualTypes) <- readResultColumns api ptr
    matchResultWithHeader types names expected api ptr actualNames actualTypes

-- | Variant of 'matchResult' accepting already-read column names and types.
matchResultWithHeader ::
    [CInt] ->
    [Text] ->
    ByteString ->
    DuckDBAPI ->
    Ptr DuckDBResult ->
    [Text] ->
    [CInt] ->
    IO (Either DuckDBError Bool)
matchResultWithHeader types names expected api ptr actualNames actualTypes
    | not (all supportedType actualTypes) =
        pure (Left (DuckDBUnsupportedResultTypes (map fromIntegral actualTypes)))
    | actualTypes /= types || actualNames /= names =
        pure (Right False)
    | otherwise = evalContT do
        (count, columnTypes) <- ContT $ \use -> withArrayLen types (curry use)
        (buf, size) <- ContT $ BSU.unsafeUseAsCStringLen expected
        liftIO do
            result <-
                matchRows
                    (apiFetchChunk api)
                    (apiDestroyChunk api)
                    (apiChunkSize api)
                    (apiChunkVector api)
                    (apiVectorData api)
                    (apiValidity api)
                    (apiStringData api)
                    ptr
                    columnTypes
                    (fromIntegral count)
                    buf
                    (fromIntegral size)
            pure $ case result of
                -1 -> Left (DuckDBInfrastructureError "Could not allocate DuckDB result matcher")
                _ -> Right (result == 1)

-- | Pre-encode an authoritative 'ResultSet' into the binary layout consumed by 'matchResult'.
encodeExpectedRows :: ResultSet -> Maybe ByteString
encodeExpectedRows (ResultSet columns types rows)
    | length types /= length columns || not (all supportedType types) = Nothing
    | otherwise = do
        encoded <- traverse encodeRow rows
        pure
            (BL.toStrict (BB.toLazyByteString (BB.word64LE (fromIntegral (length rows)) <> mconcat encoded)))
    where
        encodeRow row
            | length row == length types = mconcat <$> zipWithM encodeCell types row
            | otherwise = Nothing
        encodeCell _ Null = Just (BB.word8 1)
        encodeCell ty value = (BB.word8 0 <>) <$> encodeValue ty value
        encodeValue 1 (Bool value) = Just (BB.word64LE (if value then 1 else 0))
        encodeValue ty value@Number {}
            | ty >= 2 && ty <= 5 = BB.int64LE <$> decodeValue value
            | ty >= 6 && ty <= 9 = BB.word64LE <$> decodeValue value
            | ty == 16 && valueInRange (-(2 ^ (127 :: Int))) (2 ^ (127 :: Int)) value =
                wide <$> decodeValue value
            | ty == 32 && valueInRange 0 (2 ^ (128 :: Int)) value = wide <$> decodeValue value
        encodeValue ty (Object fields)
            | KM.size fields == 1
            , ty == 10 || ty == 11 =
                BB.word64LE <$> (KM.lookup "float64_bits" fields >>= decodeValue)
            | KM.size fields == 1
            , ty == 18 =
                encodeBytes . BS.pack <$> (KM.lookup "blob" fields >>= decodeValue)
        encodeValue 17 (String value) = Just (encodeBytes (TE.encodeUtf8 value))
        encodeValue _ _ = Nothing
        encodeBytes bytes = BB.word64LE (fromIntegral (BS.length bytes)) <> BB.byteString bytes
        wide (value :: Integer) = BB.word64LE (fromIntegral value) <> BB.word64LE (fromIntegral (value `shiftR` 64))
        valueInRange lo hi (Number n) = lo <= n && n < hi
        valueInRange _ _ _ = False
        decodeValue :: (FromJSON a) => Value -> Maybe a
        decodeValue value = case fromJSON value of
            Success x -> Just x
            Error _ -> Nothing
