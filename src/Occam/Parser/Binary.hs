{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Binary decoding for compact FFI transport from sqlparser frontend.
module Occam.Parser.Binary (
    binaryMagic,
    decodeBinaryResponse,
) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64, Word8)
import GHC.Exts (Int (..), Int#, isTrue#, (+#), (-#), (<#), (<=#))
import Occam.Core (
    AlterChange (..),
    AlterTable (..),
    BarrierReason (..),
    BarrierScope (..),
    BeginPolicy (..),
    CatalogOp (..),
    ColumnDef (..),
    ConflictAction (..),
    Constraint (..),
    ConstraintRef (..),
    ConstraintValidity (..),
    DataWrite (..),
    DeleteScope (..),
    DmlEvent (..),
    DropMode (..),
    DropObject (..),
    DropOp (..),
    ForeignKey (..),
    Ident (..),
    IndexDef (..),
    IndexRef (..),
    IndexRole (..),
    IndexSchemaPlacement (..),
    IndexUniqueness (..),
    Name (..),
    NameRef (..),
    ObjectRef (..),
    OnCommitAction (..),
    PartitionLink (..),
    ReadFact (..),
    ReadMode (..),
    ReferentialAction (..),
    RefreshOp (..),
    RelationKind (..),
    RenameOp (..),
    SchemaDef (..),
    ScopedBarrier (..),
    SearchPathEntry (..),
    SearchPathValue (..),
    SemanticFacts (..),
    SequenceDef (..),
    SequenceWrite (..),
    SessionEffect (..),
    SessionInit (..),
    SessionScope (..),
    SessionValue (..),
    SourceColumn (..),
    TableDef (..),
    TriggerBody (..),
    TriggerDef (..),
    TriggerEffect (..),
    TriggerEvent (..),
    TriggerRef (..),
    TriggerTiming (..),
    TruncateBehavior (..),
    TruncateIdentity (..),
    TxEffect (..),
    TypeDef (..),
    UpdateScope (..),
    ViewColumnLineage (..),
    ViewDef (..),
    WriteKind (..),
 )
import Occam.Facts (
    ColumnSite (..),
    ColumnSiteKind (..),
    ColumnTarget (..),
    ParserEdit (..),
    ParserEditCategory (..),
 )
import Occam.Parser.Native (NativeFacts (..), NativeResponse (..), NativeUnit (..))

-- | Magic header bytes prefixing every binary FFI response (OCCM v2).
binaryMagic :: ByteString
binaryMagic = "OCCM\x02"

-- | Zero-allocation unboxed direct-style decoder over a strict 'ByteString' cursor.
newtype Decoder a = Decoder
    { unDec :: ByteString -> Int# -> Int# -> (# Text | (# a, Int# #) #)
    }

instance Functor Decoder where
    {-# INLINE fmap #-}
    fmap :: (a -> b) -> Decoder a -> Decoder b
    fmap f (Decoder g) = Decoder $ \bs len pos -> case g bs len pos of
        (# e | #) -> (# e | #)
        (# | (# a, pos' #) #) -> (# | (# f a, pos' #) #)

instance Applicative Decoder where
    {-# INLINE pure #-}
    pure :: a -> Decoder a
    pure a = Decoder $ \_ _ pos -> (# | (# a, pos #) #)

    {-# INLINE (<*>) #-}
    (<*>) :: Decoder (a -> b) -> Decoder a -> Decoder b
    Decoder gf <*> Decoder ga = Decoder $ \bs len pos -> case gf bs len pos of
        (# e | #) -> (# e | #)
        (# | (# f, pos1 #) #) -> case ga bs len pos1 of
            (# e | #) -> (# e | #)
            (# | (# a, pos2 #) #) -> (# | (# f a, pos2 #) #)

instance Monad Decoder where
    {-# INLINE (>>=) #-}
    (>>=) :: Decoder a -> (a -> Decoder b) -> Decoder b
    Decoder ga >>= f = Decoder $ \bs len pos -> case ga bs len pos of
        (# e | #) -> (# e | #)
        (# | (# a, pos1 #) #) -> unDec (f a) bs len pos1

instance MonadFail Decoder where
    {-# INLINE fail #-}
    fail :: String -> Decoder a
    fail msg = Decoder $ \_ _ _ -> (# T.pack msg | #)

-- | Decode a binary native parser response payload (without the magic header).
decodeBinaryResponse :: ByteString -> Either Text NativeResponse
decodeBinaryResponse bytes =
    let !(I# len) = BS.length bytes
     in case unDec getResponse bytes len 0# of
            (# err | #) -> Left err
            (# | (# response, pos #) #) ->
                if I# pos == BS.length bytes
                    then Right response
                    else Left "trailing unparsed bytes in binary response"

{-# INLINE getWord8 #-}
getWord8 :: Decoder Word8
getWord8 = Decoder $ \bs len pos ->
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
getWord32le = Decoder $ \bs len pos ->
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

{-# INLINE getInt64 #-}
getInt64 :: Decoder Integer
getInt64 = Decoder $ \bs len pos ->
    if isTrue# (pos +# 8# <=# len)
        then
            let !i = I# pos
                !w0 = fromIntegral (BSU.unsafeIndex bs i) :: Word64
                !w1 = fromIntegral (BSU.unsafeIndex bs (i + 1)) :: Word64
                !w2 = fromIntegral (BSU.unsafeIndex bs (i + 2)) :: Word64
                !w3 = fromIntegral (BSU.unsafeIndex bs (i + 3)) :: Word64
                !w4 = fromIntegral (BSU.unsafeIndex bs (i + 4)) :: Word64
                !w5 = fromIntegral (BSU.unsafeIndex bs (i + 5)) :: Word64
                !w6 = fromIntegral (BSU.unsafeIndex bs (i + 6)) :: Word64
                !w7 = fromIntegral (BSU.unsafeIndex bs (i + 7)) :: Word64
                !w =
                    w0
                        .|. (w1 `shiftL` 8)
                        .|. (w2 `shiftL` 16)
                        .|. (w3 `shiftL` 24)
                        .|. (w4 `shiftL` 32)
                        .|. (w5 `shiftL` 40)
                        .|. (w6 `shiftL` 48)
                        .|. (w7 `shiftL` 56)
                !i64 = fromIntegral w :: Int64
                !val = fromIntegral i64
             in (# | (# val, pos +# 8# #) #)
        else (# "unexpected end of binary stream" | #)

{-# INLINE getText #-}
getText :: Decoder Text
getText = Decoder $ \bs len pos -> case unDec getInt32 bs len pos of
    (# err | #) -> (# err | #)
    (# | (# n@(I# n#), pos1 #) #) ->
        if n < 0 || n > I# (len -# pos1)
            then (# "unexpected end of binary stream" | #)
            else
                if n == 0
                    then (# | (# T.empty, pos1 #) #)
                    else
                        let !slice = BSU.unsafeTake n (BSU.unsafeDrop (I# pos1) bs)
                         in case TE.decodeUtf8' slice of
                                Left exc -> (# T.pack ("invalid UTF-8 in binary stream: " ++ show exc) | #)
                                Right !txt -> (# | (# txt, pos1 +# n# #) #)

-- | Intern common static edit labels without UTF-8 decoding or Text allocation.
{-# INLINE decodeEditLabel #-}
decodeEditLabel :: ByteString -> Int# -> Int# -> (# Text | (# Text, Int# #) #)
decodeEditLabel bs len pos =
    if isTrue# (pos +# 4# <=# len)
        then
            let !n@(I# n#) = fromIntegral (decodeWord32LE bs (I# pos))
                !pos1 = pos +# 4#
             in if n < 0 || n > I# (len -# pos1)
                    then (# "unexpected end of binary stream" | #)
                    else
                        if n == 0
                            then (# | (# T.empty, pos1 #) #)
                            else
                                let !slice = BSU.unsafeTake n (BSU.unsafeDrop (I# pos1) bs)
                                 in case internEditLabel n slice of
                                        Just !txt -> (# | (# txt, pos1 +# n# #) #)
                                        Nothing -> case TE.decodeUtf8' slice of
                                            Left exc -> (# T.pack ("invalid UTF-8 in binary stream: " ++ show exc) | #)
                                            Right !txt -> (# | (# txt, pos1 +# n# #) #)
        else (# "unexpected end of binary stream" | #)

-- | Intern common edit replacements without retaining slices of the FFI buffer.
{-# INLINE decodeEditReplacement #-}
decodeEditReplacement :: ByteString -> Int# -> Int# -> (# Text | (# ByteString, Int# #) #)
decodeEditReplacement bs len pos =
    if isTrue# (pos +# 4# <=# len)
        then
            let !n@(I# n#) = fromIntegral (decodeWord32LE bs (I# pos))
                !pos1 = pos +# 4#
             in if n < 0 || n > I# (len -# pos1)
                    then (# "unexpected end of binary stream" | #)
                    else case n of
                        0 -> (# | (# BS.empty, pos1 #) #)
                        1 -> case BSU.unsafeIndex bs (I# pos1) of
                            0x30 -> (# | (# repZero, pos1 +# 1# #) #)
                            0x31 -> (# | (# repOne, pos1 +# 1# #) #)
                            _ ->
                                let !rep = BS.copy (BSU.unsafeTake 1 (BSU.unsafeDrop (I# pos1) bs))
                                 in (# | (# rep, pos1 +# 1# #) #)
                        3 ->
                            let !slice = BSU.unsafeTake 3 (BSU.unsafeDrop (I# pos1) bs)
                                !rep = if slice == repQuotedZero then repQuotedZero else BS.copy slice
                             in (# | (# rep, pos1 +# 3# #) #)
                        4 ->
                            let !slice = BSU.unsafeTake 4 (BSU.unsafeDrop (I# pos1) bs)
                                !rep = if slice == repNull then repNull else BS.copy slice
                             in (# | (# rep, pos1 +# 4# #) #)
                        _ ->
                            let !slice = BSU.unsafeTake n (BSU.unsafeDrop (I# pos1) bs)
                                !rep = BS.copy slice
                             in (# | (# rep, pos1 +# n# #) #)
        else (# "unexpected end of binary stream" | #)

repZero, repOne, repQuotedZero, repNull :: ByteString
repZero = "0"
repOne = "1"
repQuotedZero = "'0'"
repNull = "NULL"

internEditLabel :: Int -> ByteString -> Maybe Text
internEditLabel 10 s
    | s == "remove-cte" = Just lblRemoveCte
    | otherwise = Nothing
internEditLabel 11 s
    | s == "remove-with" = Just lblRemoveWith
    | s == "remove-join" = Just lblRemoveJoin
    | otherwise = Nothing
internEditLabel 12 s
    | s == "remove-where" = Just lblRemoveWhere
    | s == "remove-limit" = Just lblRemoveLimit
    | s == "remove-fetch" = Just lblRemoveFetch
    | otherwise = Nothing
internEditLabel 13 s
    | s == "remove-offset" = Just lblRemoveOffset
    | s == "remove-having" = Just lblRemoveHaving
    | s == "simplify-cast" = Just lblSimplifyCast
    | s == "simplify-like" = Just lblSimplifyLike
    | otherwise = Nothing
internEditLabel 14 s
    | s == "remove-in_list" = Just lblRemoveInList
    | otherwise = Nothing
internEditLabel 15 s
    | s == "remove-order_by" = Just lblRemoveOrderBy
    | s == "remove-group_by" = Just lblRemoveGroupBy
    | s == "remove-distinct" = Just lblRemoveDistinct
    | s == "simplify-exists" = Just lblSimplifyExists
    | s == "simplify-is_null" = Just lblSimplifyIsNull
    | otherwise = Nothing
internEditLabel 16 s
    | s == "simplify-literal" = Just lblSimplifyLiteral
    | s == "simplify-between" = Just lblSimplifyBetween
    | s == "simplify-in_list" = Just lblSimplifyInList
    | s == "remove-cte-chunk" = Just lblRemoveCteChunk
    | s == "remove-returning" = Just lblRemoveReturning
    | otherwise = Nothing
internEditLabel 17 s
    | s == "remove-values_row" = Just lblRemoveValuesRow
    | s == "remove-tuple_item" = Just lblRemoveTupleItem
    | s == "remove-list-chunk" = Just lblRemoveListChunk
    | otherwise = Nothing
internEditLabel 18 s
    | s == "simplify-data_type" = Just lblSimplifyDataType
    | s == "simplify-case_else" = Just lblSimplifyCaseElse
    | otherwise = Nothing
internEditLabel 19 s
    | s == "simplify-in_subquery" = Just lblSimplifyInSubquery
    | s == "simplify-case_result" = Just lblSimplifyCaseResult
    | otherwise = Nothing
internEditLabel 20 s
    | s == "simplify-case_literal" = Just lblSimplifyCaseLiteral
    | s == "simplify-cast_literal" = Just lblSimplifyCastLiteral
    | s == "remove-in_list-chunk" = Just lblRemoveInListChunk
    | s == "remove-from-relation" = Just lblRemoveFromRelation
    | s == "remove-window_frame" = Just lblRemoveWindowFrame
    | otherwise = Nothing
internEditLabel 22 s
    | s == "simplify-scalar-subquery" = Just lblSimplifyScalarSubquery
    | s == "remove-tuple_item-chunk" = Just lblRemoveTupleItemChunk
    | s == "remove-table_constraint" = Just lblRemoveTableConstraint
    | otherwise = Nothing
internEditLabel 23 s
    | s == "remove-values_row-chunk" = Just lblRemoveValuesRowChunk
    | s == "remove-column_constraint" = Just lblRemoveColumnConstraint
    | s == "remove-column_create_def" = Just lblRemoveColumnCreateDef
    | s == "remove-window_order_by" = Just lblRemoveWindowOrderBy
    | s == "remove-update_assignment" = Just lblRemoveUpdateAssignment
    | s == "simplify-unary_expression" = Just lblSimplifyUnaryExpression
    | otherwise = Nothing
internEditLabel 24 s
    | s == "remove-select_expression" = Just lblRemoveSelectExpression
    | s == "remove-function_argument" = Just lblRemoveFunctionArgument
    | s == "remove-column_insert_item" = Just lblRemoveColumnInsertItem
    | s == "remove-column_values_item" = Just lblRemoveColumnValuesItem
    | s == "simplify-function_literal" = Just lblSimplifyFunctionLiteral
    | otherwise = Nothing
internEditLabel 25 s
    | s == "simplify-binary_expression" = Just lblSimplifyBinaryExpression
    | s == "simplify-window_spec_empty" = Just lblSimplifyWindowSpecEmpty
    | s == "remove-order_by_expression" = Just lblRemoveOrderByExpression
    | s == "remove-group_by_expression" = Just lblRemoveGroupByExpression
    | otherwise = Nothing
internEditLabel 26 s
    | s == "remove-from-relation-chunk" = Just lblRemoveFromRelationChunk
    | s == "remove-window_partition_by" = Just lblRemoveWindowPartitionBy
    | s == "simplify-set_operation_left" = Just lblSimplifySetOperationLeft
    | otherwise = Nothing
internEditLabel 27 s
    | s == "simplify-set_operation_right" = Just lblSimplifySetOperationRight
    | s == "remove-returning_expression" = Just lblRemoveReturningExpression
    | otherwise = Nothing
internEditLabel 28 s
    | s == "simplify-function_to_argument" = Just lblSimplifyFunctionToArgument
    | s == "remove-distinct_on_expression" = Just lblRemoveDistinctOnExpression
    | otherwise = Nothing
internEditLabel 30 s
    | s == "remove-select_expression-chunk" = Just lblRemoveSelectExpressionChunk
    | s == "remove-function_argument-chunk" = Just lblRemoveFunctionArgumentChunk
    | otherwise = Nothing
internEditLabel 33 s
    | s == "simplify-parenthesized_expression" = Just lblSimplifyParenthesizedExpression
    | otherwise = Nothing
internEditLabel 34 s
    | s == "remove-window_order_by_expression" = Just lblRemoveWindowOrderByExpression
    | otherwise = Nothing
internEditLabel 37 s
    | s == "remove-window_partition_by_expression" = Just lblRemoveWindowPartitionByExpression
    | otherwise = Nothing
internEditLabel _ _ = Nothing

lblRemoveCte
    , lblRemoveWith
    , lblRemoveJoin
    , lblRemoveWhere
    , lblRemoveLimit
    , lblRemoveFetch
    , lblRemoveOffset
    , lblRemoveHaving
    , lblSimplifyCast
    , lblSimplifyLike
    , lblRemoveInList
    , lblRemoveOrderBy
    , lblRemoveGroupBy
    , lblRemoveDistinct
    , lblSimplifyExists
    , lblSimplifyIsNull
    , lblSimplifyLiteral
    , lblSimplifyBetween
    , lblSimplifyInList
    , lblRemoveCteChunk
    , lblRemoveReturning
    , lblRemoveValuesRow
    , lblRemoveTupleItem
    , lblRemoveListChunk
    , lblSimplifyDataType
    , lblSimplifyCaseElse
    , lblSimplifyInSubquery
    , lblSimplifyCaseResult
    , lblSimplifyCaseLiteral
    , lblSimplifyCastLiteral
    , lblRemoveInListChunk
    , lblRemoveFromRelation
    , lblRemoveWindowFrame
    , lblSimplifyScalarSubquery
    , lblRemoveTupleItemChunk
    , lblRemoveTableConstraint
    , lblRemoveValuesRowChunk
    , lblRemoveColumnConstraint
    , lblRemoveColumnCreateDef
    , lblRemoveWindowOrderBy
    , lblRemoveUpdateAssignment
    , lblSimplifyUnaryExpression
    , lblRemoveSelectExpression
    , lblRemoveFunctionArgument
    , lblRemoveColumnInsertItem
    , lblRemoveColumnValuesItem
    , lblSimplifyFunctionLiteral
    , lblSimplifyBinaryExpression
    , lblSimplifyWindowSpecEmpty
    , lblRemoveOrderByExpression
    , lblRemoveGroupByExpression
    , lblRemoveFromRelationChunk
    , lblRemoveWindowPartitionBy
    , lblSimplifySetOperationLeft
    , lblSimplifySetOperationRight
    , lblRemoveReturningExpression
    , lblSimplifyFunctionToArgument
    , lblRemoveDistinctOnExpression
    , lblRemoveSelectExpressionChunk
    , lblRemoveFunctionArgumentChunk
    , lblSimplifyParenthesizedExpression
    , lblRemoveWindowOrderByExpression
    , lblRemoveWindowPartitionByExpression ::
        Text
lblRemoveCte = "remove-cte"
lblRemoveWith = "remove-with"
lblRemoveJoin = "remove-join"
lblRemoveWhere = "remove-where"
lblRemoveLimit = "remove-limit"
lblRemoveFetch = "remove-fetch"
lblRemoveOffset = "remove-offset"
lblRemoveHaving = "remove-having"
lblSimplifyCast = "simplify-cast"
lblSimplifyLike = "simplify-like"
lblRemoveInList = "remove-in_list"
lblRemoveOrderBy = "remove-order_by"
lblRemoveGroupBy = "remove-group_by"
lblRemoveDistinct = "remove-distinct"
lblSimplifyExists = "simplify-exists"
lblSimplifyIsNull = "simplify-is_null"
lblSimplifyLiteral = "simplify-literal"
lblSimplifyBetween = "simplify-between"
lblSimplifyInList = "simplify-in_list"
lblRemoveCteChunk = "remove-cte-chunk"
lblRemoveReturning = "remove-returning"
lblRemoveValuesRow = "remove-values_row"
lblRemoveTupleItem = "remove-tuple_item"
lblRemoveListChunk = "remove-list-chunk"
lblSimplifyDataType = "simplify-data_type"
lblSimplifyCaseElse = "simplify-case_else"
lblSimplifyInSubquery = "simplify-in_subquery"
lblSimplifyCaseResult = "simplify-case_result"
lblSimplifyCaseLiteral = "simplify-case_literal"
lblSimplifyCastLiteral = "simplify-cast_literal"
lblRemoveInListChunk = "remove-in_list-chunk"
lblRemoveFromRelation = "remove-from-relation"
lblRemoveWindowFrame = "remove-window_frame"
lblSimplifyScalarSubquery = "simplify-scalar-subquery"
lblRemoveTupleItemChunk = "remove-tuple_item-chunk"
lblRemoveTableConstraint = "remove-table_constraint"
lblRemoveValuesRowChunk = "remove-values_row-chunk"
lblRemoveColumnConstraint = "remove-column_constraint"
lblRemoveColumnCreateDef = "remove-column_create_def"
lblRemoveWindowOrderBy = "remove-window_order_by"
lblRemoveUpdateAssignment = "remove-update_assignment"
lblSimplifyUnaryExpression = "simplify-unary_expression"
lblRemoveSelectExpression = "remove-select_expression"
lblRemoveFunctionArgument = "remove-function_argument"
lblRemoveColumnInsertItem = "remove-column_insert_item"
lblRemoveColumnValuesItem = "remove-column_values_item"
lblSimplifyFunctionLiteral = "simplify-function_literal"
lblSimplifyBinaryExpression = "simplify-binary_expression"
lblSimplifyWindowSpecEmpty = "simplify-window_spec_empty"
lblRemoveOrderByExpression = "remove-order_by_expression"
lblRemoveGroupByExpression = "remove-group_by_expression"
lblRemoveFromRelationChunk = "remove-from-relation-chunk"
lblRemoveWindowPartitionBy = "remove-window_partition_by"
lblSimplifySetOperationLeft = "simplify-set_operation_left"
lblSimplifySetOperationRight = "simplify-set_operation_right"
lblRemoveReturningExpression = "remove-returning_expression"
lblSimplifyFunctionToArgument = "simplify-function_to_argument"
lblRemoveDistinctOnExpression = "remove-distinct_on_expression"
lblRemoveSelectExpressionChunk = "remove-select_expression-chunk"
lblRemoveFunctionArgumentChunk = "remove-function_argument-chunk"
lblSimplifyParenthesizedExpression = "simplify-parenthesized_expression"
lblRemoveWindowOrderByExpression = "remove-window_order_by_expression"
lblRemoveWindowPartitionByExpression = "remove-window_partition_by_expression"

{-# INLINE getVec #-}
getVec :: Decoder a -> Decoder [a]
getVec (Decoder item) = Decoder $ \bs len pos -> case unDec getInt32 bs len pos of
    (# err | #) -> (# err | #)
    (# | (# I# count, pos0 #) #) ->
        if isTrue# (count <# 0#) || isTrue# (len -# pos0 <# count)
            then (# "invalid vector length in binary stream" | #)
            else go count pos0
        where
            go 0# p = (# | (# [], p #) #)
            go k p = case item bs len p of
                (# err | #) -> (# err | #)
                (# | (# x, p1 #) #) -> case go (k -# 1#) p1 of
                    (# err | #) -> (# err | #)
                    (# | (# xs, p2 #) #) -> (# | (# x : xs, p2 #) #)

{-# INLINE getIdent #-}
getIdent :: Decoder Ident
getIdent = Ident <$> getText

{-# INLINE getName #-}
getName :: Decoder Name
getName = Name <$> getVec getIdent

{-# INLINE getNameRef #-}
getNameRef :: Decoder NameRef
getNameRef = NameRef <$> getName <*> getBool

getSearchPathEntry :: Decoder SearchPathEntry
getSearchPathEntry = do
    tag <- getWord8
    case tag of
        0 -> pure SearchPathCurrentUser
        1 -> SearchPathName <$> getName
        _ -> fail ("invalid SearchPathEntry tag: " ++ show tag)

getSearchPathValue :: Decoder SearchPathValue
getSearchPathValue = do
    tag <- getWord8
    case tag of
        0 -> pure SearchPathDefault
        1 -> SearchPathExplicit <$> getVec getSearchPathEntry
        _ -> fail ("invalid SearchPathValue tag: " ++ show tag)

{-# INLINE getOptional #-}
getOptional :: Decoder a -> Decoder (Maybe a)
getOptional getVal = do
    tag <- getWord8
    case tag of
        0 -> pure Nothing
        1 -> Just <$> getVal
        _ -> fail ("invalid Maybe tag: " ++ show tag)

getSessionInit :: Decoder SessionInit
getSessionInit = do
    nsPrefix <- getVec getIdent
    searchPath <- getVec getSearchPathEntry
    currentUser <- getOptional getIdent
    autocommit <- getBool
    fkeys <- getBool
    varPairs <- getVec ((,) <$> getText <*> getText)
    pure
        SessionInit
            { sessionNamespacePrefix = nsPrefix
            , sessionSearchPath = searchPath
            , sessionCurrentUser = currentUser
            , sessionAutocommit = autocommit
            , sessionForeignKeyChecks = fkeys
            , sessionVariables = Map.fromList varPairs
            }

getReadMode :: Decoder ReadMode
getReadMode = do
    code <- getWord8
    case code of
        0 -> pure ReadData
        1 -> pure ReadSchema
        2 -> pure ReadRowCount
        3 -> pure ReadRowExistence
        _ -> fail ("invalid ReadMode tag: " ++ show code)

getReadFact :: Decoder ReadFact
getReadFact = ReadFact <$> getNameRef <*> getReadMode <*> getVec getIdent <*> getBool

getRelationKind :: Decoder RelationKind
getRelationKind = do
    code <- getWord8
    case code of
        0 -> pure TableRelation
        1 -> pure ViewRelation
        2 -> pure MaterializedViewRelation
        _ -> fail ("invalid RelationKind tag: " ++ show code)

getReferentialAction :: Decoder ReferentialAction
getReferentialAction = do
    code <- getWord8
    case code of
        0 -> pure NoAction
        1 -> pure Restrict
        2 -> pure Cascade
        3 -> pure SetNull
        4 -> pure SetDefault
        _ -> fail ("invalid ReferentialAction tag: " ++ show code)

getOnCommitAction :: Decoder OnCommitAction
getOnCommitAction = do
    code <- getWord8
    case code of
        0 -> pure OnCommitDeleteRows
        1 -> pure OnCommitDrop
        _ -> fail ("invalid OnCommitAction tag: " ++ show code)

getIndexSchemaPlacement :: Decoder IndexSchemaPlacement
getIndexSchemaPlacement = do
    code <- getWord8
    case code of
        0 -> pure IndexTableSchema
        1 -> pure IndexCurrentSchema
        _ -> fail ("invalid IndexSchemaPlacement tag: " ++ show code)

getForeignKey :: Decoder ForeignKey
getForeignKey =
    ForeignKey
        <$> getOptional getIdent
        <*> getVec getIdent
        <*> getNameRef
        <*> getVec getIdent
        <*> getReferentialAction
        <*> getReferentialAction
        <*> getBool
        <*> getBool

getConstraint :: Decoder Constraint
getConstraint = do
    tag <- getWord8
    case tag of
        0 -> PrimaryKeyConstraint <$> getOptional getIdent <*> getVec getIdent
        1 -> UniqueConstraint <$> getOptional getIdent <*> getVec getIdent
        2 -> ForeignKeyConstraint <$> getForeignKey
        3 ->
            CheckConstraint
                <$> getOptional getIdent
                <*> getVec getIdent
                <*> ((\notValid -> if notValid then ConstraintNotValid else ConstraintValid) <$> getBool)
        4 ->
            IndexConstraint
                <$> getOptional getIdent
                <*> getVec getIdent
                <*> ((\unique -> if unique then IndexUnique else IndexNonUnique) <$> getBool)
        5 ->
            UsingIndexConstraint
                <$> getOptional getIdent
                <*> getIdent
                <*> ((\primary -> if primary then PrimaryIndex else SecondaryIndex) <$> getBool)
                <*> getIndexSchemaPlacement
        6 -> OtherConstraint <$> getOptional getIdent
        _ -> fail ("invalid Constraint tag: " ++ show tag)

getColumnDef :: Decoder ColumnDef
getColumnDef =
    ColumnDef
        <$> getIdent
        <*> getBool
        <*> getBool
        <*> getBool
        <*> getVec getForeignKey

getTableDef :: Decoder TableDef
getTableDef =
    TableDef
        <$> getName
        <*> getBool
        <*> getBool
        <*> getVec getColumnDef
        <*> getVec getConstraint
        <*> getVec getReadFact
        <*> getOptional getNameRef
        <*> getOptional getNameRef
        <*> getVec getNameRef
        <*> getOptional getOnCommitAction
        <*> getBool
        <*> getBool

getSourceColumn :: Decoder SourceColumn
getSourceColumn = SourceColumn <$> getNameRef <*> getIdent

getViewColumnLineage :: Decoder ViewColumnLineage
getViewColumnLineage = ViewColumnLineage <$> getIdent <*> getOptional (getVec getSourceColumn)

getViewDef :: Decoder ViewDef
getViewDef =
    ViewDef
        <$> getName
        <*> getBool
        <*> getBool
        <*> getVec getIdent
        <*> getVec getReadFact
        <*> getBool
        <*> getBool
        <*> getVec getViewColumnLineage
        <*> getVec getSourceColumn

getIndexRef :: Decoder IndexRef
getIndexRef = do
    tag <- getWord8
    case tag of
        0 -> NamedIndex <$> getName
        1 -> TableIndex <$> getNameRef <*> getIdent
        _ -> fail ("invalid IndexRef tag: " ++ show tag)

getIndexDef :: Decoder IndexDef
getIndexDef =
    IndexDef
        <$> getIndexRef
        <*> getNameRef
        <*> getVec getIdent
        <*> getBool
        <*> getBool
        <*> getBool
        <*> getBool
        <*> getIndexSchemaPlacement

getTriggerRef :: Decoder TriggerRef
getTriggerRef = do
    tag <- getWord8
    case tag of
        0 -> NamedTrigger <$> getName
        1 -> TableTrigger <$> getNameRef <*> getIdent
        _ -> fail ("invalid TriggerRef tag: " ++ show tag)

getConstraintRef :: Decoder ConstraintRef
getConstraintRef = ConstraintRef <$> getNameRef <*> getIdent

getTriggerTiming :: Decoder TriggerTiming
getTriggerTiming = do
    code <- getWord8
    case code of
        0 -> pure TriggerBefore
        1 -> pure TriggerAfter
        2 -> pure TriggerInsteadOf
        _ -> fail ("invalid TriggerTiming tag: " ++ show code)

getTriggerEvent :: Decoder TriggerEvent
getTriggerEvent = do
    tag <- getWord8
    case tag of
        0 -> pure TriggerInsert
        1 -> TriggerUpdate <$> getVec getIdent
        2 -> pure TriggerDelete
        3 -> pure TriggerTruncate
        _ -> fail ("invalid TriggerEvent tag: " ++ show tag)

getDmlEvent :: Decoder DmlEvent
getDmlEvent = do
    tag <- getWord8
    case tag of
        0 -> DmlInsert <$> getNameRef <*> getVec getIdent
        1 -> DmlUpdate <$> getNameRef <*> getVec getIdent
        2 -> DmlMerge <$> getNameRef <*> getVec getIdent
        3 ->
            DmlDelete
                <$> getNameRef
                <*> ((\allRows -> if allRows then DeleteAllRows else DeleteFilteredRows) <$> getBool)
        4 ->
            DmlTruncate
                <$> getNameRef
                <*> ((\cascade -> if cascade then TruncateCascade else TruncateRestrict) <$> getBool)
        _ -> fail ("invalid DmlEvent tag: " ++ show tag)

getSequenceWrite :: Decoder SequenceWrite
getSequenceWrite = SequenceWrite <$> getNameRef <*> getBool

getSessionScope :: Decoder SessionScope
getSessionScope = do
    code <- getWord8
    case code of
        0 -> pure ScopeSession
        1 -> pure ScopeTransaction
        2 -> pure ScopeLocal
        3 -> pure ScopeGlobal
        _ -> fail ("invalid SessionScope tag: " ++ show code)

getSessionValue :: Decoder SessionValue
getSessionValue = do
    tag <- getWord8
    case tag of
        0 -> SessionText <$> getText
        1 -> SessionBool <$> getBool
        2 -> SessionInteger <$> getInt64
        3 -> SessionNames <$> getVec getName
        4 -> pure SessionDefault
        _ -> fail ("invalid SessionValue tag: " ++ show tag)

getSessionEffect :: Decoder SessionEffect
getSessionEffect = do
    tag <- getWord8
    case tag of
        0 -> SetVariable <$> getSessionScope <*> getName <*> getSessionValue
        1 -> SetSearchPath <$> getSessionScope <*> getSearchPathValue
        2 -> SetNamespace <$> getSessionScope <*> getVec getIdent
        3 -> SetSessionUser <$> getSessionScope <*> getIdent
        4 -> ResetSessionUser <$> getSessionScope
        5 -> SetCurrentUser <$> getSessionScope <*> getIdent
        6 -> ResetCurrentUser <$> getSessionScope
        7 -> SetAutocommit <$> getSessionScope <*> getBool
        8 -> SetForeignKeyChecks <$> getSessionScope <*> getBool
        9 -> SetSqliteForeignKeys <$> getBool
        10 -> ResetForeignKeyChecks <$> getSessionScope
        11 -> ResetVariable <$> getSessionScope <*> getName
        12 -> ResetAll <$> getSessionScope
        _ -> fail ("invalid SessionEffect tag: " ++ show tag)

getBeginPolicy :: Decoder BeginPolicy
getBeginPolicy = do
    code <- getWord8
    case code of
        0 -> pure BeginError
        1 -> pure BeginWarn
        2 -> pure BeginCommitActive
        3 -> pure BeginIgnore
        _ -> fail ("invalid BeginPolicy tag: " ++ show code)

getTxEffect :: Decoder TxEffect
getTxEffect = do
    tag <- getWord8
    case tag of
        0 -> TxBegin <$> getBeginPolicy
        1 -> TxCommit <$> getBool
        2 -> pure TxRollbackAll
        3 -> TxRollbackTo <$> getIdent
        4 -> TxSavepoint <$> getIdent
        5 -> TxReleaseSavepoint <$> getIdent
        6 -> TxSetReadOnly <$> getBool
        _ -> fail ("invalid TxEffect tag: " ++ show tag)

getObjectRef :: Decoder ObjectRef
getObjectRef = do
    tag <- getWord8
    case tag of
        0 -> ObjectRelation <$> getNameRef
        1 -> ObjectIndex <$> getIndexRef
        2 -> ObjectTrigger <$> getTriggerRef
        3 -> ObjectSequence <$> getNameRef
        4 -> ObjectType <$> getName
        5 -> ObjectSchema <$> getName
        6 -> ObjectConstraint <$> getConstraintRef
        _ -> fail ("invalid ObjectRef tag: " ++ show tag)

getBarrierScope :: Decoder BarrierScope
getBarrierScope = do
    tag <- getWord8
    case tag of
        0 -> pure GlobalScope
        1 -> RelationScope <$> getNameRef
        2 -> SchemaScope <$> getName
        3 -> ObjectScope <$> getObjectRef
        4 -> pure SessionStateScope
        5 -> pure TransactionStateScope
        _ -> fail ("invalid BarrierScope tag: " ++ show tag)

getBarrierReason :: Decoder BarrierReason
getBarrierReason = do
    code <- getWord8
    case code of
        0 -> pure SyntaxErrorBarrier
        1 -> pure UnknownStatementBarrier
        2 -> pure UnknownRelationBarrier
        3 -> pure UnknownDDLBarrier
        4 -> pure UnknownDMLBarrier
        5 -> pure UnknownDropBarrier
        6 -> pure UnknownAlterBarrier
        7 -> pure UnknownViewBarrier
        8 -> pure UnknownIndexBarrier
        9 -> pure UnknownTriggerBarrier
        10 -> pure UnknownTruncateBarrier
        11 -> pure UnknownSessionEffectBarrier
        12 -> pure UnsupportedExpressionBarrier
        13 -> pure UnsupportedConflictClauseBarrier
        14 -> pure UnsupportedTriggerBodyBarrier
        15 -> pure UnsupportedTransactionBarrier
        16 -> pure UnresolvedViewSourceBarrier
        17 -> pure WriteThroughViewBarrier
        18 -> pure OpaqueTriggerBodyBarrier
        19 -> pure UnmodeledRelationSourceBarrier
        _ -> fail ("invalid BarrierReason tag: " ++ show code)

getScopedBarrier :: Decoder ScopedBarrier
getScopedBarrier = ScopedBarrier <$> getBarrierScope <*> getBarrierReason <*> getOptional getText

getTriggerEffect :: Decoder TriggerEffect
getTriggerEffect = do
    tag <- getWord8
    case tag of
        0 -> TriggerDml <$> getDmlEvent
        1 -> TriggerRead <$> getReadFact
        2 -> TriggerWriteSequence <$> getSequenceWrite
        3 -> TriggerSession <$> getSessionEffect
        4 -> TriggerBarrier <$> getScopedBarrier
        _ -> fail ("invalid TriggerEffect tag: " ++ show tag)

getTriggerBody :: Decoder TriggerBody
getTriggerBody = do
    tag <- getWord8
    case tag of
        0 -> TriggerInline <$> getVec getTriggerEffect
        1 -> TriggerFunction <$> getName <*> getVec getText
        2 -> pure TriggerOpaque
        _ -> fail ("invalid TriggerBody tag: " ++ show tag)

getTriggerDef :: Decoder TriggerDef
getTriggerDef =
    TriggerDef
        <$> getTriggerRef
        <*> getNameRef
        <*> getTriggerTiming
        <*> getVec getTriggerEvent
        <*> getBool
        <*> getVec getReadFact
        <*> getTriggerBody
        <*> getOptional getNameRef
        <*> getBool
        <*> getBool
        <*> getBool

getConflictAction :: Decoder ConflictAction
getConflictAction = do
    code <- getWord8
    case code of
        0 -> pure ConflictError
        1 -> pure ConflictIgnore
        2 -> pure ConflictReplace
        3 -> pure ConflictUpdate
        _ -> fail ("invalid ConflictAction tag: " ++ show code)

getWriteKind :: Decoder WriteKind
getWriteKind = do
    tag <- getWord8
    case tag of
        0 -> InsertWrite <$> getConflictAction
        1 ->
            UpdateWrite . (\allRows -> if allRows then UpdateAllRows else UpdateFilteredRows) <$> getBool
        2 ->
            DeleteWrite . (\allRows -> if allRows then DeleteAllRows else DeleteFilteredRows) <$> getBool
        3 -> pure MergeWrite
        4 ->
            (TruncateWrite . (\cascade -> if cascade then TruncateCascade else TruncateRestrict) <$> getBool)
                <*> ((\restart -> if restart then RestartIdentity else ContinueIdentity) <$> getBool)
        _ -> fail ("invalid WriteKind tag: " ++ show tag)

getDataWrite :: Decoder DataWrite
getDataWrite =
    DataWrite
        <$> getNameRef
        <*> getWriteKind
        <*> getVec getIdent
        <*> getVec getIdent
        <*> getVec getReadFact
        <*> getBool

getAlterChange :: Decoder AlterChange
getAlterChange = do
    tag <- getWord8
    case tag of
        0 -> AddColumn <$> getColumnDef <*> getVec getConstraint
        1 -> DropColumn <$> getIdent <*> getBool
        2 -> AddConstraint <$> getConstraint
        3 -> DropConstraint <$> getIdent <*> getBool
        4 -> RenameConstraint <$> getIdent <*> getIdent
        5 -> ValidateConstraint <$> getIdent
        6 -> DropIndexChange <$> getIdent
        7 -> AlterColumn <$> getIdent <*> getText <*> getVec getIdent <*> getBool
        8 -> OtherChange <$> getText
        _ -> fail ("invalid AlterChange tag: " ++ show tag)

getAlterTable :: Decoder AlterTable
getAlterTable = AlterTable <$> getNameRef <*> getVec getAlterChange

getPartitionLink :: Decoder PartitionLink
getPartitionLink = PartitionLink <$> getNameRef <*> getNameRef

getDropMode :: Decoder DropMode
getDropMode = do
    code <- getWord8
    case code of
        0 -> pure DropRestrict
        1 -> pure DropCascade
        _ -> fail ("invalid DropMode tag: " ++ show code)

getDropObject :: Decoder DropObject
getDropObject = do
    tag <- getWord8
    case tag of
        0 -> DropRelation <$> getNameRef <*> getRelationKind
        1 -> DropIndex <$> getIndexRef
        2 -> DropTrigger <$> getTriggerRef
        3 -> DropSequence <$> getNameRef
        4 -> DropType <$> getName
        5 -> DropSchema <$> getName
        _ -> fail ("invalid DropObject tag: " ++ show tag)

getDropOp :: Decoder DropOp
getDropOp = DropOp <$> getVec getDropObject <*> getDropMode

getRenameOp :: Decoder RenameOp
getRenameOp = do
    tag <- getWord8
    case tag of
        0 -> RenameRelation <$> getNameRef <*> getName <*> getIndexSchemaPlacement
        1 -> RenameColumnOp <$> getNameRef <*> getIdent <*> getIdent
        _ -> fail ("invalid RenameOp tag: " ++ show tag)

getRefreshOp :: Decoder RefreshOp
getRefreshOp = RefreshOp <$> getNameRef <*> getReadMode

getCatalogOp :: Decoder CatalogOp
getCatalogOp = do
    tag <- getWord8
    case tag of
        0 -> DefineTableOp <$> getTableDef
        1 -> DefineViewOp <$> getViewDef
        2 -> DefineIndexOp <$> getIndexDef
        3 -> DefineTriggerOp <$> getTriggerDef
        4 -> DefineSequenceOp <$> (SequenceDef <$> getName <*> getBool <*> getBool)
        5 -> DefineTypeOp . TypeDef <$> getName
        6 -> DefineSchemaOp . SchemaDef <$> getName
        7 -> ReadOp <$> getReadFact
        8 -> WriteDataOp <$> getDataWrite
        9 -> WriteSequenceOp <$> getSequenceWrite
        10 -> AlterTableOp <$> getAlterTable
        11 -> AttachPartitionOp <$> getPartitionLink
        12 -> DetachPartitionOp <$> getPartitionLink
        13 -> AttachInheritOp <$> getPartitionLink
        14 -> DetachInheritOp <$> getPartitionLink
        15 -> DropCatalogOp <$> getDropOp
        16 -> RenameOp <$> getRenameOp
        17 -> RefreshCatalogOp <$> getRefreshOp
        _ -> fail ("invalid CatalogOp tag: " ++ show tag)

{-# INLINE decodeEditCategory #-}
decodeEditCategory :: Word8 -> (# Text | ParserEditCategory #)
decodeEditCategory 0 = (# | EditExpression #)
decodeEditCategory 1 = (# | EditClause #)
decodeEditCategory 2 = (# | EditConstraint #)
decodeEditCategory 3 = (# | EditList #)
decodeEditCategory 4 = (# | EditColumn #)
decodeEditCategory 5 = (# | EditType #)
decodeEditCategory 6 = (# | EditLiteral #)
decodeEditCategory code = (# T.pack ("unknown parser edit category code: " ++ show code) | #)

{-# INLINE decodeParserEdit #-}
decodeParserEdit :: ByteString -> Int# -> Int# -> (# Text | (# ParserEdit, Int# #) #)
decodeParserEdit bs len pos =
    if isTrue# (pos +# 8# <=# len)
        then
            let !i = I# pos
                !start = fromIntegral (decodeWord32LE bs i)
                !end = fromIntegral (decodeWord32LE bs (i + 4))
             in case decodeEditReplacement bs len (pos +# 8#) of
                    (# err | #) -> (# err | #)
                    (# | (# rep, posRep #) #) ->
                        if isTrue# (posRep <# len)
                            then
                                let !rawCat = BSU.unsafeIndex bs (I# posRep)
                                    !topProj = (rawCat .&. 0x80) /= 0
                                    !tailClause = (rawCat .&. 0x40) /= 0
                                 in case decodeEditCategory (rawCat .&. 0x3f) of
                                        (# err | #) -> (# err | #)
                                        (# | cat #) -> case decodeEditLabel bs len (posRep +# 1#) of
                                            (# err | #) -> (# err | #)
                                            (# | (# label, posNext #) #) ->
                                                (#
                                                    | (#
                                                        ParserEdit
                                                            { parserEditStart = start
                                                            , parserEditEnd = end
                                                            , parserEditReplacement = rep
                                                            , parserEditCategory = cat
                                                            , parserEditLabel = label
                                                            , parserEditTopProjection = topProj
                                                            , parserEditTailClause = tailClause
                                                            }
                                                        , posNext
                                                      #)
                                                #)
                            else (# "unexpected end of binary stream" | #)
        else (# "unexpected end of binary stream" | #)

getParserEditVec :: Decoder [ParserEdit]
getParserEditVec = getVec (Decoder decodeParserEdit)

{-# INLINE getSemanticFacts #-}
getSemanticFacts :: Decoder SemanticFacts
getSemanticFacts =
    SemanticFacts
        <$> getVec getCatalogOp
        <*> getVec getSessionEffect
        <*> getVec getTxEffect
        <*> getVec getTxEffect
        <*> getVec getScopedBarrier

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
