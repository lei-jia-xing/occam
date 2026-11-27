-- | Source-range and UTF-8 boundary validation for parsed column targets.
module Occam.Parser.Validate (
    isUtf8Boundary,
    validateColumns,
) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.IntMap.Strict qualified as IntMap
import Occam.Core (Span (..), Statement (..))
import Occam.Facts (
    ColumnSite (..),
    ColumnTarget (..),
    StatementUnit (..),
 )
import Occam.Parser.Types (ParserError (..), ParserErrorKind (..))

-- | True when the byte offset is zero, end-of-input, or a UTF-8 lead byte.
{-# INLINE isUtf8Boundary #-}
isUtf8Boundary :: ByteString -> Int -> Int -> Bool
isUtf8Boundary source sourceLength offset =
    offset == 0 || offset == sourceLength || BSU.unsafeIndex source offset .&. 0xc0 /= 0x80

-- | Validate response-level column targets against the statement units they cite.
validateColumns ::
    ByteString -> [StatementUnit] -> [ColumnTarget] -> Either ParserError [ColumnTarget]
validateColumns _ _ [] = Right []
validateColumns source units targets = do
    let unitBounds = IntMap.fromList (zip [1 ..] (map (statementBounds . unitStatement) units))
        badRange = Left (ParserError ParserInvalidEditRange "frontend returned an invalid column span")
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
