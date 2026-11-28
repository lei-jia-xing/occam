-- | Shared analysis context, identifier normalization, and source slicing for frontend passes.
module Occam.Frontend.Context (
    -- * Identifier folding rules
    IdentifierClass (..),
    IdentifierRules (..),
    identifierRulesFor,
    foldFor,
    foldIdentText,

    -- * Per-statement analysis context
    AnalysisContext (..),
    makeAnalysisContext,

    -- * Source spans and slicing
    toAbsoluteSpan,
    toAbsoluteCoreSpan,
    sliceSource,
    unsafeSliceSource,
    unitSource,

    -- * AST identifier and object-name normalization
    unquoteNamePart,
    unquoteLiteral,
    canonicalIdent,
    canonicalIdentWith,
    canonicalIdentText,
    isVariableIdent,
    isUserVariableIdent,
    nameParts,
    namePartsWith,
    nameFromAST,
    nameFromASTWith,
    stripTempSchema,
    relationFromParts,
    relationRef,
    definitionName,

    -- * Literal identifier parsing (session config, sequence names)
    splitIdentifierList,
    literalIdentifierPart,
    literalRelationName,

    -- * Statement helpers
    isExplainAnalyzeOption,
    unwrapExplainAnalyze,
) where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Occam.Core.Schema qualified as Core
import Occam.Core.Types qualified as Core
import Occam.Frontend.AST.Expr qualified as AST
import Occam.Frontend.AST.Statement qualified as AST
import Occam.Frontend.Lexer (SpannedToken)
import Occam.Frontend.Span (
    ByteSpan (..),
    Located (..),
    sliceSpan,
    toCoreSpan,
    unsafeSliceSpan,
 )
import Occam.Parser.Dialect (
    CaseFolding (..),
    DialectProfile (..),
    IdentifierProfile (..),
    applyCaseFolding,
    dialectProfile,
 )
import Occam.Parser.Types (
    ParseOptions (..),
    SQLDialect (..),
    ServerConfig (..),
 )

-- ---------------------------------------------------------------------------
-- Identifier folding rules
-- ---------------------------------------------------------------------------

-- | Semantic class of an SQL identifier for dialect- and server-specific folding.
data IdentifierClass
    = -- | Table, view, schema, database, or trigger-target relation names.
      IdentRelation
    | -- | Column names and projection aliases.
      IdentColumn
    | -- | Functions, savepoints, constraints, variables, and other identifiers.
      IdentOther
    deriving (Eq, Ord, Show)

{- | Effective identifier case-folding rules for a dialect and 'ServerConfig'.

Mirrors Rust @IdentifierRules@: base unquoted/quoted folding comes from the
'DialectProfile', refined by MySQL/MariaDB @lower_case_table_names@ and explicit
per-class case-sensitivity overrides in 'ServerConfig'.
-}
data IdentifierRules = IdentifierRules
    { rulesUnquoted :: !CaseFolding
    , rulesQuoted :: !CaseFolding
    , rulesRelationUnquoted :: !(Maybe CaseFolding)
    , rulesRelationQuoted :: !(Maybe CaseFolding)
    , rulesColumnUnquoted :: !(Maybe CaseFolding)
    , rulesColumnQuoted :: !(Maybe CaseFolding)
    }
    deriving (Eq, Show)

-- | Compute identifier folding rules for @dialect@, applying all 'ServerConfig' overrides.
identifierRulesFor :: SQLDialect -> ServerConfig -> IdentifierRules
identifierRulesFor dialect server =
    IdentifierRules
        { rulesUnquoted = identUnquotedFolding identProfile
        , rulesQuoted = quotedBase
        , rulesRelationUnquoted = relationOverride
        , rulesRelationQuoted = relationOverride
        , rulesColumnUnquoted = columnOverride
        , rulesColumnQuoted = columnOverride
        }
    where
        identProfile = dialectIdentifiers (dialectProfile dialect)
        boolFolding sensitive = if sensitive then FoldPreserve else FoldLower
        mysqlRelationOverride
            | identSupportsLowerCaseTableNames identProfile =
                Just (if serverLowerCaseTableNames server == 0 then FoldPreserve else FoldLower)
            | otherwise = Nothing
        relationOverride =
            case serverTableNamesCaseSensitive server of
                Just sensitive -> Just (boolFolding sensitive)
                Nothing -> mysqlRelationOverride
        columnOverride = fmap boolFolding (serverColumnNamesCaseSensitive server)
        quotedBase =
            case serverQuotedIdentifiersCaseSensitive server of
                Just sensitive -> boolFolding sensitive
                Nothing -> identQuotedFolding identProfile

-- | Resolve the effective 'CaseFolding' for an 'IdentifierClass' and quoting status.
foldFor :: IdentifierRules -> IdentifierClass -> Bool -> CaseFolding
foldFor rules cls quoted = fromMaybe fallback specific
    where
        fallback = if quoted then rulesQuoted rules else rulesUnquoted rules
        specific = case (cls, quoted) of
            (IdentRelation, True) -> rulesRelationQuoted rules
            (IdentRelation, False) -> rulesRelationUnquoted rules
            (IdentColumn, True) -> rulesColumnQuoted rules
            (IdentColumn, False) -> rulesColumnUnquoted rules
            (IdentOther, _) -> Nothing

-- | Fold raw unquoted identifier text into a canonical 'Core.Ident'.
foldIdentText :: IdentifierRules -> IdentifierClass -> Bool -> Text -> Core.Ident
foldIdentText rules cls quoted raw =
    Core.Ident (applyCaseFolding (foldFor rules cls quoted) raw)

-- ---------------------------------------------------------------------------
-- AnalysisContext
-- ---------------------------------------------------------------------------

{- | Immutable per-unit context passed to 'Occam.Frontend.Semantics',
'Occam.Frontend.Edits', and 'Occam.Frontend.Columns'.

When constructed by 'Occam.Frontend.Pipeline.parseFrontend', AST 'ByteSpan's
already carry absolute offsets into 'ctxSource' and 'ctxAstSpanShift' is @0@.
Always pass AST spans through 'toAbsoluteSpan' (or 'sliceSource') before
emitting 'Occam.Facts.ParserEdit's or slicing 'ctxSource' so compatibility
callers that re-parse an isolated unit slice also work transparently.
-}
data AnalysisContext = AnalysisContext
    { ctxDialect :: !SQLDialect
    -- ^ Active SQL dialect (equal to @dialectId ctxProfile@).
    , ctxProfile :: !DialectProfile
    -- ^ Complete dialect capability profile.
    , ctxIdentRules :: !IdentifierRules
    -- ^ Effective identifier folding rules including 'ServerConfig' overrides.
    , ctxSource :: !ByteString
    -- ^ Complete UTF-8 validated script source bytes.
    , ctxUnitSpan :: !ByteSpan
    -- ^ Absolute half-open byte span @[start, end)@ of this unit in 'ctxSource'.
    , ctxUnitOffset :: !Int
    -- ^ Absolute start byte offset of this unit (equal to @spanStart ctxUnitSpan@).
    , ctxAstSpanShift :: !Int
    -- ^ Offset added by 'toAbsoluteSpan' to convert AST spans into 'ctxSource' offsets (@0@ in full-script parses).
    , ctxUnitTokens :: !(Vector SpannedToken)
    -- ^ Token slice (including trivia) covering this statement unit.
    , ctxChunkEdits :: !Bool
    -- ^ Emit coarse-to-fine list-chunk deletions alongside single-item edits.
    , ctxColumnIR :: !Bool
    -- ^ Collect per-unit column geometry and emit single-point column edits (@parseColumnIR || parseColumnBundles@).
    , ctxSyntaxEdits :: !Bool
    -- ^ Emit per-edit intra-statement syntax IR.
    , ctxSyntaxOnly :: !Bool
    -- ^ Skip semantic dependency extraction when only syntax edits are needed (ignored when 'ctxColumnIR' is True).
    , ctxPureTableEnabled :: !Bool
    -- ^ Whether pure built-in functions may lift routine fences; disabled script-wide on any routine DDL or parse error.
    }

-- | Build an 'AnalysisContext' for one statement unit from full-script 'ParseOptions'.
makeAnalysisContext ::
    ParseOptions ->
    ByteString ->
    ByteSpan ->
    Vector SpannedToken ->
    Bool ->
    AnalysisContext
makeAnalysisContext options source unitSpan tokens pureTableEnabled =
    AnalysisContext
        { ctxDialect = dialect
        , ctxProfile = dialectProfile dialect
        , ctxIdentRules = identifierRulesFor dialect (parseServerConfig options)
        , ctxSource = source
        , ctxUnitSpan = unitSpan
        , ctxUnitOffset = spanStart unitSpan
        , ctxAstSpanShift = 0
        , ctxUnitTokens = tokens
        , ctxChunkEdits = parseChunkEdits options
        , ctxColumnIR = parseColumnIR options || parseColumnBundles options
        , ctxSyntaxEdits = parseSyntaxEdits options
        , ctxSyntaxOnly = parseSyntaxOnly options
        , ctxPureTableEnabled = pureTableEnabled
        }
    where
        dialect = parseDialect options

-- ---------------------------------------------------------------------------
-- Spans and source slicing
-- ---------------------------------------------------------------------------

-- | Convert an AST 'ByteSpan' to an absolute 'ByteSpan' in 'ctxSource'.
{-# INLINE toAbsoluteSpan #-}
toAbsoluteSpan :: AnalysisContext -> ByteSpan -> ByteSpan
toAbsoluteSpan ctx (ByteSpan s e) = ByteSpan (shift + s) (shift + e)
    where
        !shift = ctxAstSpanShift ctx

-- | Convert an AST 'ByteSpan' to an absolute 'Core.Span' in 'ctxSource'.
{-# INLINE toAbsoluteCoreSpan #-}
toAbsoluteCoreSpan :: AnalysisContext -> ByteSpan -> Core.Span
toAbsoluteCoreSpan ctx = toCoreSpan . toAbsoluteSpan ctx

-- | Checked slice of 'ctxSource' at an AST 'ByteSpan'.
sliceSource :: AnalysisContext -> ByteSpan -> Maybe ByteString
sliceSource ctx sp = sliceSpan (toAbsoluteSpan ctx sp) (ctxSource ctx)

-- | Zero-copy slice of 'ctxSource' at an AST 'ByteSpan'. Caller must ensure valid UTF-8 bounds.
{-# INLINE unsafeSliceSource #-}
unsafeSliceSource :: AnalysisContext -> ByteSpan -> ByteString
unsafeSliceSource ctx sp = unsafeSliceSpan (toAbsoluteSpan ctx sp) (ctxSource ctx)

-- | Zero-copy slice of 'ctxSource' covering the entire current statement unit.
{-# INLINE unitSource #-}
unitSource :: AnalysisContext -> ByteString
unitSource ctx = unsafeSliceSpan (ctxUnitSpan ctx) (ctxSource ctx)

-- ---------------------------------------------------------------------------
-- Identifier and name normalization
-- ---------------------------------------------------------------------------

-- | Decode an AST 'AST.NamePart', stripping outer quotes and unescaping doubled delimiters.
unquoteNamePart :: AST.NamePart -> Text
unquoteNamePart (AST.NamePart raw quoted)
    | not quoted = text
    | otherwise = case (T.uncons text, T.unsnoc (T.drop 1 text)) of
        (Just ('"', _), Just (interior, '"')) -> T.replace "\"\"" "\"" interior
        (Just ('`', _), Just (interior, '`')) -> T.replace "``" "`" interior
        (Just ('[', _), Just (interior, ']')) -> interior
        _ -> text
    where
        text = TE.decodeUtf8 raw

{- | Unquote a SQL quoted string or identifier literal, returning 'Nothing' if
delimiters do not match or backslash escapes are present.
-}
unquoteLiteral :: Text -> Maybe Text
unquoteLiteral value = do
    (open, rest) <- T.uncons value
    (contents, close) <- T.unsnoc rest
    let expectedClose = if open == '[' then ']' else open
    guard (close == expectedClose && open `elem` ['[', '`', '"', '\''])
    guard (not (T.any (== '\\') contents))
    if open == '['
        then Just contents
        else Just (T.replace (T.replicate 2 (T.singleton close)) (T.singleton close) contents)

-- | Normalize an AST 'AST.NamePart' using explicit 'IdentifierRules' and 'IdentifierClass'.
canonicalIdentWith :: IdentifierRules -> IdentifierClass -> AST.NamePart -> Core.Ident
canonicalIdentWith rules cls part =
    foldIdentText rules cls (AST.namePartQuoted part) (unquoteNamePart part)

-- | Normalize an AST 'AST.NamePart' under the context's identifier folding rules.
canonicalIdent :: AnalysisContext -> IdentifierClass -> AST.NamePart -> Core.Ident
canonicalIdent ctx = canonicalIdentWith (ctxIdentRules ctx)

-- | Normalize an AST 'AST.NamePart' and unwrap its canonical 'Text'.
canonicalIdentText :: AnalysisContext -> IdentifierClass -> AST.NamePart -> Text
canonicalIdentText ctx cls part =
    let Core.Ident txt = canonicalIdent ctx cls part
     in txt

-- | True if @part@ is an unquoted @\@@-prefixed variable (@@\@user@@ or @\@\@sys@).
isVariableIdent :: AST.NamePart -> Bool
isVariableIdent (AST.NamePart raw quoted) =
    not quoted && BS.isPrefixOf "@" raw

-- | True if @part@ is an unquoted user-defined variable (@@\@name@@, not @\@\@sys@ or bare @\@@).
isUserVariableIdent :: AST.NamePart -> Bool
isUserVariableIdent (AST.NamePart raw quoted) =
    not quoted
        && BS.isPrefixOf "@" raw
        && not (BS.isPrefixOf "@@" raw)
        && BS.length raw > 1

{- | Fold every component of an AST 'AST.Name' into canonical 'Core.Ident's.

Matches Rust @names::name_parts@: rejects empty names and names with more than
4 parts; all leading qualifier parts fold as 'IdentRelation' and only the
terminal part folds as @cls@.
-}
namePartsWith :: IdentifierRules -> IdentifierClass -> AST.Name -> Maybe [Core.Ident]
namePartsWith rules cls (AST.Name parts)
    | null parts || n > 4 = Nothing
    | otherwise = Just (zipWith foldPart [0 ..] parts)
    where
        n = length parts
        foldPart !idx (L _ part) =
            canonicalIdentWith rules (if idx + 1 < n then IdentRelation else cls) part

-- | Context-bound version of 'namePartsWith'.
nameParts :: AnalysisContext -> IdentifierClass -> AST.Name -> Maybe [Core.Ident]
nameParts ctx = namePartsWith (ctxIdentRules ctx)

-- | Convert an AST 'AST.Name' into a canonical 'Core.Name' using explicit rules.
nameFromASTWith :: IdentifierRules -> IdentifierClass -> AST.Name -> Maybe Core.Name
nameFromASTWith rules cls = fmap Core.Name . namePartsWith rules cls

-- | Convert an AST 'AST.Name' into a canonical 'Core.Name'.
nameFromAST :: AnalysisContext -> IdentifierClass -> AST.Name -> Maybe Core.Name
nameFromAST ctx = nameFromASTWith (ctxIdentRules ctx)

{- | Strip leading dialect temporary schema qualifiers (@pg_temp@ / @pg_temp_*@
in PostgreSQL, @temp@ in SQLite and DuckDB) and report whether one was removed.
-}
stripTempSchema :: SQLDialect -> [Core.Ident] -> ([Core.Ident], Bool)
stripTempSchema dialect parts = case parts of
    Core.Ident first : rest
        | not (null rest) && isTempSchema first -> (rest, True)
    _ -> (parts, False)
    where
        isTempSchema name = case dialect of
            PostgreSQL -> name == "pg_temp" || T.isPrefixOf "pg_temp_" name
            SQLite -> name == "temp"
            DuckDB -> name == "temp"
            _ -> False

-- | Build a 'Core.NameRef' from folded identifier parts, detecting temporary schema prefixes.
relationFromParts :: SQLDialect -> [Core.Ident] -> Core.NameRef
relationFromParts dialect parts =
    let (stripped, isTemp) = stripTempSchema dialect parts
     in Core.NameRef (Core.Name stripped) isTemp

-- | Resolve an AST 'AST.Name' into a 'Core.NameRef' with folding and temp-schema detection.
relationRef :: AnalysisContext -> AST.Name -> Maybe Core.NameRef
relationRef ctx name =
    relationFromParts (ctxDialect ctx) <$> nameParts ctx IdentRelation name

{- | Resolve an object definition name into @(canonicalName, isTemporary)@,
combining explicit @TEMPORARY@/@TEMP@ declaration with temp-schema qualifiers.
-}
definitionName :: AnalysisContext -> Bool -> AST.Name -> Maybe (Core.Name, Bool)
definitionName ctx declaredTemp name = do
    parts <- nameParts ctx IdentRelation name
    let (stripped, schemaTemp) = stripTempSchema (ctxDialect ctx) parts
    pure (Core.Name stripped, declaredTemp || schemaTemp)

-- ---------------------------------------------------------------------------
-- Literal identifier parsing (session configuration and sequence names)
-- ---------------------------------------------------------------------------

{- | Split a literal identifier list on @separator@, ignoring separators inside
double-quoted identifiers. Doubled quotes (@""@) inside quotes escape a literal quote.
Returns 'Nothing' on an unterminated quote.
-}
splitIdentifierList :: Char -> Text -> Maybe [Text]
splitIdentifierList separator value = go False T.empty [] (T.unpack value)
    where
        go True _ _ [] = Nothing
        go False cur acc [] = Just (reverse (T.reverse cur : acc))
        go inQuotes cur acc (c : rest)
            | c == '"' = case (inQuotes, rest) of
                (True, '"' : rest') -> go True (T.cons '"' (T.cons '"' cur)) acc rest'
                _ -> go (not inQuotes) (T.cons '"' cur) acc rest
            | c == separator && not inQuotes =
                go False T.empty (T.reverse cur : acc) rest
            | otherwise = go inQuotes (T.cons c cur) acc rest

{- | Parse one identifier part from a literal string (such as a @search_path@
entry or a PostgreSQL sequence name literal), returning @(unescapedText, isQuoted)@.

Double-quoted identifiers preserve internal whitespace, unescape @""@ to @"@,
and reject empty @""@. Unquoted identifiers are trimmed and may not contain
quotes or whitespace.
-}
literalIdentifierPart :: Text -> Maybe (Text, Bool)
literalIdentifierPart raw =
    let trimmed = T.strip raw
     in if T.null trimmed
            then Nothing
            else case T.stripPrefix "\"" trimmed of
                Just afterOpen -> do
                    interior <- T.stripSuffix "\"" afterOpen
                    unescaped <- unescapeDouble interior
                    guard (not (T.null unescaped))
                    Just (unescaped, True)
                Nothing -> do
                    guard (not (T.any (== '"') trimmed) && not (T.any isSpace trimmed))
                    Just (trimmed, False)
    where
        unescapeDouble = fmap T.pack . go . T.unpack
        go [] = Just []
        go ('"' : '"' : rest) = ('"' :) <$> go rest
        go ('"' : _) = Nothing
        go (c : rest) = (c :) <$> go rest

{- | Parse a dotted textual relation name (e.g. PostgreSQL
@nextval(\'tenant."MixedCase"."seq"\')@) into a canonical 'Core.Name'.
-}
literalRelationName :: IdentifierRules -> Text -> Maybe Core.Name
literalRelationName rules value = do
    rawParts <- splitIdentifierList '.' value
    Core.Name <$> traverse parseOne rawParts
    where
        parseOne part = do
            (identText, quoted) <- literalIdentifierPart part
            pure (foldIdentText rules IdentRelation quoted identText)

-- ---------------------------------------------------------------------------
-- Statement helpers
-- ---------------------------------------------------------------------------

-- | True when an @EXPLAIN@ option enables @ANALYZE@ (which actually executes the inner statement).
isExplainAnalyzeOption :: AST.ExplainOption -> Bool
isExplainAnalyzeOption opt =
    locValue (AST.explainOptionName opt) == "ANALYZE"
        && case fmap locValue (AST.explainOptionValue opt) of
            Nothing -> True
            Just val -> val `notElem` ["FALSE", "OFF", "0"]

{- | Unwrap @EXPLAIN ANALYZE ...@ recursively so semantic analysis, edit
generation, and column extraction analyze the executed inner statement directly.
-}
unwrapExplainAnalyze :: AST.Statement -> AST.Statement
unwrapExplainAnalyze = \case
    AST.StatementExplain (L _ explain)
        | any (isExplainAnalyzeOption . locValue) (AST.explainOptions explain) ->
            unwrapExplainAnalyze (locValue (AST.explainStatement explain))
    stmt -> stmt
