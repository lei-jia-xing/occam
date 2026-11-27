{- | Pure-Haskell intra-statement 'ParserEdit' generation from the frontend AST.

Emits exact-span clause, list, constraint, type, expression, and literal edits,
including ClickHouse @PREWHERE@ / @SETTINGS@ and DuckDB @QUALIFY@ removals.
-}
module Occam.Frontend.Edits (
    statementEdits,
) where

import Occam.Facts (ParserEdit)
import Occam.Frontend.AST qualified as AST
import Occam.Frontend.Context (AnalysisContext (..))
import Occam.Frontend.Edits.Core (runEditM)
import Occam.Frontend.Edits.Stmt (visitStatement)

{- | Generate intra-statement 'ParserEdit' candidates for one parsed statement.

Returns @[]@ when 'ctxSyntaxEdits' is 'False'. Respects 'ctxChunkEdits' for
coarse-to-fine list-chunk edits.
-}
statementEdits :: AnalysisContext -> AST.Statement -> [ParserEdit]
statementEdits ctx stmt
    | not (ctxSyntaxEdits ctx) = []
    | otherwise = runEditM ctx (visitStatement stmt)
