-- | Binary decoding for compact FFI transport from sqlparser frontend.
module Occam.Parser.Binary (
    binaryMagic,
    decodeBinaryResponse,
) where

import Control.Monad (replicateM)
import Data.Aeson (eitherDecodeStrict')
import Data.Binary.Get (
    Get,
    getByteString,
    getWord32le,
    getWord8,
    runGetOrFail,
 )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Core (
    Ident (..),
    Name (..),
    SearchPathEntry (..),
    SemanticFacts (..),
    SessionInit (..),
 )
import Occam.Facts (
    ColumnSite (..),
    ColumnSiteKind (..),
    ColumnTarget (..),
    ParserEdit (..),
    ParserEditCategory (..),
 )
import Occam.Parser.Native (NativeFacts (..), NativeResponse (..), NativeUnit (..))

-- | Magic header bytes prefixing every binary FFI response.
binaryMagic :: ByteString
binaryMagic = "OCCM\x01"

-- | Decode a binary native parser response payload (without the magic header).
decodeBinaryResponse :: ByteString -> Either Text NativeResponse
decodeBinaryResponse bytes =
    case runGetOrFail getResponse (BL.fromStrict bytes) of
        Left (_, _, err) -> Left (T.pack err)
        Right (remaining, _, response)
            | BL.null remaining -> Right response
            | otherwise -> Left "trailing unparsed bytes in binary response"

getInt32 :: Get Int
getInt32 = fromIntegral <$> getWord32le

getText :: Get Text
getText = do
    len <- getInt32
    bytes <- getByteString len
    case TE.decodeUtf8' bytes of
        Left exc -> fail ("invalid UTF-8 in binary stream: " ++ show exc)
        Right txt -> pure txt

getIdent :: Get Ident
getIdent = Ident <$> getText

getName :: Get Name
getName = do
    count <- getInt32
    Name <$> replicateM count getIdent

getSearchPathEntry :: Get SearchPathEntry
getSearchPathEntry = do
    tag <- getWord8
    case tag of
        0 -> pure SearchPathCurrentUser
        1 -> SearchPathName <$> getName
        _ -> fail ("invalid SearchPathEntry tag: " ++ show tag)

getOptional :: Get a -> Get (Maybe a)
getOptional getVal = do
    tag <- getWord8
    case tag of
        0 -> pure Nothing
        1 -> Just <$> getVal
        _ -> fail ("invalid Maybe tag: " ++ show tag)

getSessionInit :: Get SessionInit
getSessionInit = do
    nsCount <- getInt32
    nsPrefix <- replicateM nsCount getIdent
    spCount <- getInt32
    searchPath <- replicateM spCount getSearchPathEntry
    currentUser <- getOptional getIdent
    autocommitByte <- getWord8
    let autocommit = autocommitByte /= 0
    fkeyByte <- getWord8
    let fkeys = fkeyByte /= 0
    varCount <- getInt32
    varPairs <- replicateM varCount ((,) <$> getText <*> getText)
    pure
        SessionInit
            { sessionNamespacePrefix = nsPrefix
            , sessionSearchPath = searchPath
            , sessionCurrentUser = currentUser
            , sessionAutocommit = autocommit
            , sessionForeignKeyChecks = fkeys
            , sessionVariables = Map.fromList varPairs
            }

getEditCategory :: Get ParserEditCategory
getEditCategory = do
    code <- getWord8
    case code of
        0 -> pure EditExpression
        1 -> pure EditClause
        2 -> pure EditConstraint
        3 -> pure EditList
        4 -> pure EditColumn
        5 -> pure EditType
        6 -> pure EditLiteral
        _ -> fail ("unknown parser edit category code: " ++ show code)

getParserEdit :: Get ParserEdit
getParserEdit = do
    start <- getInt32
    end <- getInt32
    repLen <- getInt32
    rep <- getByteString repLen
    category <- getEditCategory
    label <- getText
    pure
        ParserEdit
            { parserEditStart = start
            , parserEditEnd = end
            , parserEditReplacement = rep
            , parserEditCategory = category
            , parserEditLabel = label
            }

getFacts :: Get NativeFacts
getFacts = do
    semLen <- getInt32
    semantics <-
        if semLen == 0
            then pure (SemanticFacts [] [] [] [] [])
            else do
                semBytes <- getByteString semLen
                case eitherDecodeStrict' semBytes of
                    Left err -> fail ("failed to decode semantics JSON in binary stream: " ++ err)
                    Right sem -> pure sem
    editsCount <- getInt32
    edits <- replicateM editsCount getParserEdit
    pure (NativeFacts semantics edits)

getUnit :: Get NativeUnit
getUnit = do
    start <- getInt32
    end <- getInt32
    parseError <- getOptional getText
    hasFacts <- getWord8
    facts <- case hasFacts of
        0 -> pure Nothing
        1 -> Just <$> getFacts
        _ -> fail ("invalid facts flag: " ++ show hasFacts)
    pure (NativeUnit start end parseError facts)

getColumnSiteKind :: Get ColumnSiteKind
getColumnSiteKind = do
    code <- getWord8
    case code of
        0 -> pure CreateDefSite
        1 -> pure InsertListSite
        2 -> pure ValuesItemSite
        _ -> fail ("invalid column site kind code: " ++ show code)

getColumnSite :: Get ColumnSite
getColumnSite = do
    stmt <- getInt32
    start <- getInt32
    end <- getInt32
    ColumnSite stmt start end <$> getColumnSiteKind

getColumnTarget :: Get ColumnTarget
getColumnTarget = do
    tbl <- getName
    col <- getIdent
    ordinal <- getInt32
    sitesCount <- getInt32
    sites <- replicateM sitesCount getColumnSite
    blocked <- getOptional getText
    pure (ColumnTarget tbl col ordinal sites blocked)

getResponse :: Get NativeResponse
getResponse = do
    session <- getSessionInit
    unitsCount <- getInt32
    units <- replicateM unitsCount getUnit
    colsCount <- getInt32
    cols <- replicateM colsCount getColumnTarget
    pure (NativeResponse session units cols)
