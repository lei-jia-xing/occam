-- | Source-range and UTF-8 boundary validation for the decoded native response.
module Occam.Parser.Validate (
    makeUnits,
    validateColumns,
) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.IntMap.Strict qualified as IntMap
import Occam.Core (Span (..), Statement (..), StatementId (..))
import Occam.Facts (
    ColumnSite (..),
    ColumnTarget (..),
    ParserEdit (..),
    StatementFacts (..),
    StatementUnit (..),
 )
import Occam.Parser.Native (NativeFacts (..), NativeUnit (..))
import Occam.Parser.Types (ParserError (..), ParserErrorKind (..))

-- | True when the byte offset is zero, end-of-input, or a UTF-8 lead byte.
{-# INLINE isUtf8Boundary #-}
isUtf8Boundary :: ByteString -> Int -> Int -> Bool
isUtf8Boundary source sourceLength offset =
    offset == 0 || offset == sourceLength || BSU.unsafeIndex source offset .&. 0xc0 /= 0x80

makeUnits :: ByteString -> [NativeUnit] -> Either ParserError [StatementUnit]
makeUnits source = go 0 1 []
    where
        !sourceLength = BS.length source

        go _ _ acc [] = Right (reverse acc)
        go previousEnd index acc (native : rest)
            | start < 0 || start >= end || end > sourceLength || start < previousEnd =
                Left (ParserError ParserInvalidSourceRange "sqlparser returned an invalid source byte range")
            | not (isUtf8Boundary source sourceLength start && isUtf8Boundary source sourceLength end) =
                Left (ParserError ParserInvalidSourceRange "sqlparser returned a source range splitting UTF-8")
            | maybe False (not . all (validEdit start end) . nativeEdits) (nativeFacts native) =
                Left (ParserError ParserInvalidEditRange "sqlparser returned an invalid edit range")
            | otherwise =
                let !statement =
                        Statement
                            (StatementId index)
                            (Span start end)
                            (BSU.unsafeTake (end - start) (BSU.unsafeDrop start source))
                    facts = fmap makeFacts (nativeFacts native)
                 in go end (index + 1) (StatementUnit statement facts (nativeParseError native) : acc) rest
            where
                start = nativeStart native
                end = nativeEnd native
                validEdit unitStart unitEnd edit =
                    parserEditStart edit >= unitStart
                        && parserEditStart edit < parserEditEnd edit
                        && parserEditEnd edit <= unitEnd
                        && isUtf8Boundary source sourceLength (parserEditStart edit)
                        && isUtf8Boundary source sourceLength (parserEditEnd edit)

        makeFacts native = StatementFacts (nativeSemantics native) (nativeEdits native)

-- | Validate response-level column targets against the statement units they cite.
validateColumns ::
    ByteString -> [StatementUnit] -> [ColumnTarget] -> Either ParserError [ColumnTarget]
validateColumns _ _ [] = Right []
validateColumns source units targets = do
    let unitBounds = IntMap.fromList (zip [1 ..] (map (statementBounds . unitStatement) units))
        badRange = Left (ParserError ParserInvalidEditRange "sqlparser returned an invalid column span")
        checkSite site = do
            unitBound <- maybe badRange Right (IntMap.lookup (siteStmt site) unitBounds)
            if validSite unitBound site
                then Right ()
                else badRange
    mapM_ (mapM_ checkSite . targetSites) targets
    pure targets
    where
        !sourceLength = BS.length source
        statementBounds statement =
            let Span start end = stmtSpan statement in (start, end)
        validSite (unitStart, unitEnd) site =
            siteStart site >= unitStart
                && siteStart site < siteEnd site
                && siteEnd site <= unitEnd
                && isUtf8Boundary source sourceLength (siteStart site)
                && isUtf8Boundary source sourceLength (siteEnd site)
