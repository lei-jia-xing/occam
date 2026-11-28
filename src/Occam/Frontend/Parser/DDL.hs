-- | CREATE, DROP, ALTER, and column/table-definition parsers.
module Occam.Frontend.Parser.DDL (
    parseCreate,
    parseDrop,
    parseAlter,
    parseIfNotExists,
    parseIfExists,
    parseDropBehavior,
    parseColumnDef,
    parseTableConstraint,
    parseTableOption,
) where

import Occam.Frontend.Parser.DDL.AlterDrop (
    parseAlter,
    parseDrop,
    parseDropBehavior,
    parseIfExists,
 )
import Occam.Frontend.Parser.DDL.CreateObject (parseCreate)
import Occam.Frontend.Parser.DDL.CreateTable (
    parseColumnDef,
    parseIfNotExists,
    parseTableConstraint,
    parseTableOption,
 )
