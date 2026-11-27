-- | Session, transaction, SET, RESET, USE, and PRAGMA semantic extraction.
module Occam.Frontend.Semantics.Session (
    guardTransactionDialect,
    transactionFacts,
    pragmaFacts,
    useFacts,
    resetFacts,
    setFacts,
) where

import Control.Monad (zipWithM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable qualified as Foldable
import Data.Maybe (isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Core qualified as Core
import Occam.Frontend.AST qualified as AST
import Occam.Frontend.Context (
    AnalysisContext (..),
    IdentifierClass (..),
    canonicalIdent,
    foldIdentText,
    literalIdentifierPart,
    nameFromAST,
    nameParts,
    splitIdentifierList,
    unitSource,
    unquoteLiteral,
    unquoteNamePart,
    unsafeSliceSource,
 )
import Occam.Frontend.Lexer (LiteralKind (..))
import Occam.Frontend.Semantics.Pure (
    decodeStringLiteral,
    exprBool,
    exprValue,
    pragmaBool,
    pragmaSessionValue,
 )
import Occam.Frontend.Semantics.Query (
    VisitorState,
    addUnmodeledBarriers,
    analyzeNested,
    mergeNested,
    visitorReads,
 )
import Occam.Frontend.Span (ByteSpan (..), Located (..))
import Occam.Parser.Dialect (DialectProfile (..), SemanticsProfile (..))
import Occam.Parser.Types (SQLDialect (..))

-- | Dialect guard matching the transaction spellings supported across reducer paths.
guardTransactionDialect :: SQLDialect -> AST.TransactionStmt -> Bool
guardTransactionDialect dialect stmt
    | dialect == ClickHouse = False
    | dialect == SQLite && AST.transactionCommand stmt == AST.TransactionStart = False
    | dialect `elem` [DuckDB, SQLite] && isJust (AST.transactionChain stmt) = False
    | dialect `notElem` [PostgreSQL, SQLite] && AST.transactionCommand stmt == AST.TransactionEnd =
        False
    | otherwise = True

-- | Extract transaction control effects and barriers for a 'AST.TransactionStmt'.
transactionFacts ::
    AnalysisContext -> AST.TransactionStmt -> Core.SemanticFacts -> Core.SemanticFacts
transactionFacts ctx stmt baseFacts
    | not (guardTransactionDialect (ctxDialect ctx) stmt) = baseFacts
    | otherwise = case AST.transactionCommand stmt of
        AST.TransactionBegin -> beginFacts
        AST.TransactionStart -> beginFacts
        AST.TransactionCommit -> after [Core.TxCommit chain]
        AST.TransactionEnd -> after [Core.TxCommit chain]
        AST.TransactionRollback -> after (Core.TxRollbackAll : [Core.TxBegin Core.BeginIgnore | chain])
        AST.TransactionRollbackTo -> savepoint Core.TxRollbackTo
        AST.TransactionSavepoint -> savepoint Core.TxSavepoint
        AST.TransactionReleaseSavepoint -> savepoint Core.TxReleaseSavepoint
    where
        profile = ctxProfile ctx
        chain = AST.transactionChain stmt == Just True
        after effs = baseFacts {Core.factPostTx = Core.factPostTx baseFacts ++ effs}
        savepoint mk = case AST.transactionName stmt of
            Just (L _ part) -> after [mk (canonicalIdent ctx IdentOther part)]
            Nothing -> baseFacts
        modes = AST.transactionModes stmt
        beginFacts =
            baseFacts
                { Core.factPreTx =
                    Core.factPreTx baseFacts ++ [Core.TxBegin (semBeginPolicy (dialectSemantics profile))]
                , Core.factPostTx =
                    Core.factPostTx baseFacts ++ [Core.TxSetReadOnly v | AST.TransactionReadOnly v <- modes]
                , Core.factBarriers =
                    Core.factBarriers baseFacts
                        ++ [ Core.ScopedBarrier Core.GlobalScope Core.UnsupportedTransactionBarrier (Just (TE.decodeUtf8 detail))
                           | AST.TransactionUnmodeledMode detail <- modes
                           ]
                }

-- | Extract session effects and barriers for a @PRAGMA@ statement; returns @(facts, inertPragma)@.
pragmaFacts :: AnalysisContext -> AST.PragmaStmt -> Core.SemanticFacts -> (Core.SemanticFacts, Bool)
pragmaFacts ctx stmt baseFacts = case listToMaybe (reverse (AST.nameParts (locValue (AST.pragmaName stmt)))) of
    Nothing ->
        ( baseFacts
            { Core.factBarriers =
                Core.factBarriers baseFacts
                    ++ [ Core.ScopedBarrier
                            Core.GlobalScope
                            Core.UnknownSessionEffectBarrier
                            (Just (TE.decodeUtf8 (unitSource ctx)))
                       ]
            }
        , False
        )
    Just (L _ part) ->
        let name = T.toLower (unquoteNamePart part)
            mVal = AST.pragmaValue stmt
            unsupportedWith detail =
                baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [Core.ScopedBarrier Core.GlobalScope Core.UnknownSessionEffectBarrier (Just detail)]
                    }
            unsupported = unsupportedWith (TE.decodeUtf8 (unitSource ctx))
            unsupportedKnown = unsupportedWith name
         in case name of
                "foreign_key_checks" | ctxDialect ctx == SQLite -> (baseFacts, False)
                k | k `elem` ["foreign_keys", "foreign_key_checks"] -> case mVal of
                    Nothing -> (baseFacts, False)
                    Just v -> case pragmaBool v of
                        Just b
                            | ctxDialect ctx == SQLite ->
                                (baseFacts {Core.factSession = Core.factSession baseFacts ++ [Core.SetSqliteForeignKeys b]}, False)
                            | otherwise ->
                                ( baseFacts
                                    { Core.factSession =
                                        Core.factSession baseFacts ++ [Core.SetForeignKeyChecks Core.ScopeSession b]
                                    }
                                , False
                                )
                        Nothing -> (unsupportedKnown, False)
                k | k `elem` ["recursive_triggers", "defer_foreign_keys"] && isNothing mVal -> (baseFacts, False)
                k | k `elem` ["journal_mode", "synchronous", "temp_store", "cache_size", "page_size"] ->
                    case mVal of
                        Nothing -> (baseFacts, False)
                        Just v -> case pragmaSessionValue v of
                            Just sv ->
                                ( baseFacts
                                    { Core.factSession =
                                        Core.factSession baseFacts
                                            ++ [Core.SetVariable Core.ScopeSession (Core.Name [Core.Ident name]) sv]
                                    }
                                , True
                                )
                            Nothing -> (unsupportedKnown, False)
                _ -> (unsupported, False)

-- | Extract session namespace effects for a @USE@ statement.
useFacts :: AnalysisContext -> Located AST.Name -> Core.SemanticFacts -> Core.SemanticFacts
useFacts ctx (L sp astName) baseFacts =
    let validParts = case nameParts ctx IdentRelation astName of
            Just parts -> case ctxDialect ctx of
                d | d `elem` [MySQL, MariaDB, ClickHouse] && length parts == 1 -> Just parts
                DuckDB | length parts == 2 -> Just parts
                _ -> Nothing
            Nothing -> Nothing
     in case validParts of
            Just ns ->
                baseFacts
                    { Core.factSession = Core.factSession baseFacts ++ [Core.SetNamespace Core.ScopeSession ns]
                    }
            Nothing ->
                baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [ Core.ScopedBarrier
                                    Core.GlobalScope
                                    Core.UnknownSessionEffectBarrier
                                    (Just (TE.decodeUtf8 (unsafeSliceSource ctx sp)))
                               ]
                    }

-- | Extract session reset effects and barriers for a @RESET@ statement.
resetFacts :: AnalysisContext -> AST.ResetStmt -> Core.SemanticFacts -> Core.SemanticFacts
resetFacts ctx stmt baseFacts = case stmt of
    AST.ResetAll ->
        baseFacts {Core.factSession = Core.factSession baseFacts ++ [Core.ResetAll Core.ScopeSession]}
    AST.ResetRole ->
        baseFacts
            { Core.factSession = Core.factSession baseFacts ++ [Core.ResetCurrentUser Core.ScopeSession]
            }
    AST.ResetSessionAuthorization ->
        baseFacts
            { Core.factSession = Core.factSession baseFacts ++ [Core.ResetSessionUser Core.ScopeSession]
            }
    AST.ResetTarget target -> case target of
        AST.SetTargetUserVar _ -> unmodeledSpelling
        AST.SetTargetSysVar _ -> unmodeledSpelling
        AST.SetTargetSpecial _ -> unmodeledSpelling
        AST.SetTargetName (L sp astName) -> case nameFromAST ctx IdentOther astName of
            Nothing ->
                baseFacts
                    { Core.factBarriers =
                        Core.factBarriers baseFacts
                            ++ [ Core.ScopedBarrier
                                    Core.GlobalScope
                                    Core.UnknownSessionEffectBarrier
                                    (Just (TE.decodeUtf8 (unsafeSliceSource ctx sp)))
                               ]
                    }
            Just name ->
                if any (T.isPrefixOf "@" . Core.identText) (Core.nameParts name)
                    then unmodeledSpelling
                    else
                        let lastLower = maybe "" (T.toLower . Core.identText) (listToMaybe (reverse (Core.nameParts name)))
                            eff = case lastLower of
                                "search_path" -> Core.SetSearchPath Core.ScopeSession Core.SearchPathDefault
                                "foreign_keys" -> Core.ResetForeignKeyChecks Core.ScopeSession
                                "foreign_key_checks" -> Core.ResetForeignKeyChecks Core.ScopeSession
                                "current_user" -> Core.ResetCurrentUser Core.ScopeSession
                                "role" -> Core.ResetCurrentUser Core.ScopeSession
                                "session_authorization" -> Core.ResetSessionUser Core.ScopeSession
                                _ -> Core.ResetVariable Core.ScopeSession name
                         in baseFacts {Core.factSession = Core.factSession baseFacts ++ [eff]}
    where
        unmodeledSpelling =
            baseFacts
                { Core.factBarriers =
                    Core.factBarriers baseFacts
                        ++ [ Core.ScopedBarrier
                                Core.GlobalScope
                                Core.UnknownSessionEffectBarrier
                                (Just "RESET with an unmodeled variable spelling")
                           ]
                }

-- | Extract session, transaction, read, and nested DML facts for a @SET@ statement.
setFacts ::
    (AnalysisContext -> AST.Statement -> Core.SemanticFacts) ->
    AnalysisContext ->
    VisitorState ->
    AST.SetStmt ->
    Core.SemanticFacts ->
    Core.SemanticFacts
setFacts analyzeStmt ctx visitor stmt baseFacts =
    let (effs, bars, txs) = setStatementEffects ctx stmt
        readFacts = visitorReads visitor [] Core.ReadData
        nested = analyzeNested analyzeStmt ctx False (AST.StatementSet (L (ByteSpan 0 0) stmt))
        facts1 =
            baseFacts
                { Core.factOps = Core.factOps baseFacts ++ map Core.ReadOp readFacts
                , Core.factSession = Core.factSession baseFacts ++ effs
                , Core.factPostTx = Core.factPostTx baseFacts ++ txs
                , Core.factBarriers = addUnmodeledBarriers visitor [] (Core.factBarriers baseFacts ++ bars)
                }
     in mergeNested facts1 nested

setStatementEffects ::
    AnalysisContext ->
    AST.SetStmt ->
    ([Core.SessionEffect], [Core.ScopedBarrier], [Core.TxEffect])
setStatementEffects ctx stmt
    | not (null (AST.setTransactionModes stmt)) =
        let isSession = AST.setScope stmt == AST.SetScopeSession
            step (effs, bars, txs) (L _ modeBS) = case modeBS of
                "READ ONLY"
                    | isSession ->
                        ( effs
                            ++ [ Core.SetVariable
                                    Core.ScopeSession
                                    (Core.Name [Core.Ident "default_transaction_read_only"])
                                    (Core.SessionBool True)
                               ]
                        , bars
                        , txs
                        )
                    | otherwise -> (effs, bars, txs ++ [Core.TxSetReadOnly True])
                "READ WRITE"
                    | isSession ->
                        ( effs
                            ++ [ Core.SetVariable
                                    Core.ScopeSession
                                    (Core.Name [Core.Ident "default_transaction_read_only"])
                                    (Core.SessionBool False)
                               ]
                        , bars
                        , txs
                        )
                    | otherwise -> (effs, bars, txs ++ [Core.TxSetReadOnly False])
                _
                    | BS.isPrefixOf "SNAPSHOT " modeBS ->
                        ( effs
                        , bars
                            ++ [ Core.ScopedBarrier
                                    Core.GlobalScope
                                    Core.UnsupportedTransactionBarrier
                                    (Just "SET TRANSACTION SNAPSHOT")
                               ]
                        , txs
                        )
                    | otherwise ->
                        ( effs
                        , bars
                            ++ [ Core.ScopedBarrier Core.GlobalScope Core.UnsupportedTransactionBarrier (Just (TE.decodeUtf8 modeBS))
                               ]
                        , txs
                        )
         in Foldable.foldl' step ([], [], []) (AST.setTransactionModes stmt)
    | otherwise =
        let (effs, bars) = Foldable.foldl' stepItem ([], []) (AST.setItems stmt)
         in (effs, bars, [])
    where
        stepItem (effAcc, barAcc) (L _ item) =
            let (e, b) = setItemEffects ctx (AST.setScope stmt) item
             in (effAcc ++ e, barAcc ++ b)

contextScope :: AST.SetScope -> Core.SessionScope
contextScope = \case
    AST.SetScopeLocal -> Core.ScopeLocal
    AST.SetScopeGlobal -> Core.ScopeGlobal
    AST.SetScopeSession -> Core.ScopeSession
    AST.SetScopeNone -> Core.ScopeSession

systemVarScope :: Core.Name -> AST.SetScope -> Maybe Core.SessionScope
systemVarScope (Core.Name parts) keyword = case parts of
    [Core.Ident single] ->
        let isSys =
                (T.isPrefixOf "@@" single && not (T.isPrefixOf "@@@" single))
                    || not (T.isPrefixOf "@" single)
         in if isSys then Just (contextScope keyword) else Nothing
    Core.Ident qualifier : _
        | T.isPrefixOf "@@" qualifier ->
            case (T.toLower (T.dropWhile (== '@') qualifier), keyword) of
                ("global", AST.SetScopeNone) -> Just Core.ScopeGlobal
                ("global", AST.SetScopeGlobal) -> Just Core.ScopeGlobal
                ("session", AST.SetScopeNone) -> Just Core.ScopeSession
                ("session", AST.SetScopeSession) -> Just Core.ScopeSession
                _ -> Nothing
    _ -> Nothing

setItemEffects ::
    AnalysisContext -> AST.SetScope -> AST.SetItem -> ([Core.SessionEffect], [Core.ScopedBarrier])
setItemEffects ctx setScope item = case AST.setItemTarget item of
    AST.SetTargetSpecial (L _ raw) -> handleSpecial raw
    AST.SetTargetUserVar (L _ raw) ->
        if setScope /= AST.SetScopeNone
            then
                ( []
                ,
                    [ Core.ScopedBarrier
                        Core.GlobalScope
                        Core.UnknownSessionEffectBarrier
                        (Just "SET user variable with an unmodeled scope modifier")
                    ]
                )
            else
                let varIdent = canonicalIdent ctx IdentOther (AST.NamePart raw False)
                 in ( []
                    ,
                        [ Core.ScopedBarrier
                            (Core.UserVariableScope varIdent)
                            Core.UnknownSessionEffectBarrier
                            (Just ("user variable " <> Core.identText varIdent))
                        ]
                    )
    AST.SetTargetSysVar (L _ raw) ->
        maybe
            ( []
            , [Core.ScopedBarrier Core.GlobalScope Core.UnknownSessionEffectBarrier (Just (TE.decodeUtf8 raw))]
            )
            handleNamedVar
            (parseSysVarName ctx raw)
    AST.SetTargetName (L sp astName) ->
        maybe
            ( []
            ,
                [ Core.ScopedBarrier
                    Core.GlobalScope
                    Core.UnknownSessionEffectBarrier
                    (Just (TE.decodeUtf8 (unsafeSliceSource ctx sp)))
                ]
            )
            handleNamedVar
            (nameFromAST ctx IdentOther astName)
    where
        scope = contextScope setScope
        values = map locValue (AST.setItemValue item)

        handleSpecial raw = case raw of
            "NAMES" -> case values of
                [AST.ExprDefault] -> ([Core.ResetVariable Core.ScopeSession (Core.Name [Core.Ident "names"])], [])
                [cs] ->
                    (
                        [ Core.SetVariable
                            Core.ScopeSession
                            (Core.Name [Core.Ident "names"])
                            (Core.SessionText (sessionText cs))
                        ]
                    , []
                    )
                [cs, coll] ->
                    (
                        [ Core.SetVariable
                            Core.ScopeSession
                            (Core.Name [Core.Ident "names"])
                            (Core.SessionText (sessionText cs <> " COLLATE " <> sessionText coll))
                        ]
                    , []
                    )
                _ -> ([], [])
            "CHARSET" -> case values of
                [AST.ExprDefault] -> ([Core.ResetVariable Core.ScopeSession (Core.Name [Core.Ident "names"])], [])
                [cs] ->
                    (
                        [ Core.SetVariable
                            Core.ScopeSession
                            (Core.Name [Core.Ident "charset"])
                            (Core.SessionText (sessionText cs))
                        ]
                    , []
                    )
                _ -> ([], [])
            "ROLE" -> case values of
                [v] | isNoneOrDefault v -> ([Core.ResetCurrentUser scope], [])
                [AST.ExprColumn (L _ (AST.Name [L _ p]))] ->
                    ([Core.SetCurrentUser scope (canonicalIdent ctx IdentOther p)], [])
                [AST.ExprLiteral lit]
                    | Just txt <- decodeStringLiteral lit ->
                        ([Core.SetCurrentUser scope (foldIdentText (ctxIdentRules ctx) IdentOther False txt)], [])
                _ ->
                    ( []
                    ,
                        [ Core.ScopedBarrier
                            Core.GlobalScope
                            Core.UnknownSessionEffectBarrier
                            (Just (TE.decodeUtf8 (unitSource ctx)))
                        ]
                    )
            "SESSION AUTHORIZATION" -> case values of
                [v] | isNoneOrDefault v -> ([Core.ResetSessionUser scope], [])
                [AST.ExprColumn (L _ (AST.Name [L _ p]))] ->
                    ([Core.SetSessionUser scope (canonicalIdent ctx IdentOther p)], [])
                [AST.ExprLiteral lit]
                    | Just txt <- decodeStringLiteral lit ->
                        ([Core.SetSessionUser scope (foldIdentText (ctxIdentRules ctx) IdentOther False txt)], [])
                _ ->
                    ( []
                    ,
                        [ Core.ScopedBarrier
                            Core.GlobalScope
                            Core.UnknownSessionEffectBarrier
                            (Just (TE.decodeUtf8 (unitSource ctx)))
                        ]
                    )
            _ ->
                ( []
                , [Core.ScopedBarrier Core.GlobalScope Core.UnknownSessionEffectBarrier (Just (TE.decodeUtf8 raw))]
                )

        isNoneOrDefault = \case
            AST.ExprDefault -> True
            AST.ExprBareKeyword (L _ k) -> T.toLower (TE.decodeUtf8 k) == "none"
            AST.ExprColumn (L _ (AST.Name [L _ (AST.NamePart k False)])) -> T.toLower (TE.decodeUtf8 k) == "none"
            _ -> False

        sessionText e = case exprValue ctx e of
            Core.SessionText t -> t
            Core.SessionBool True -> "true"
            Core.SessionBool False -> "false"
            Core.SessionInteger n -> T.pack (show n)
            Core.SessionDefault -> "DEFAULT"
            Core.SessionNames ns -> T.intercalate "." (map Core.nameText ns)

        handleNamedVar name =
            let lastLower =
                    T.dropWhile (== '@') $
                        maybe "" (T.toLower . Core.identText) (listToMaybe (reverse (Core.nameParts name)))
             in case lastLower of
                    "search_path" -> case systemVarScope name setScope of
                        Nothing ->
                            ( []
                            ,
                                [ Core.ScopedBarrier
                                    Core.GlobalScope
                                    Core.UnknownSessionEffectBarrier
                                    (Just "SET search_path with an unmodeled scope or qualifier")
                                ]
                            )
                        Just effScope -> case searchPathValue ctx values of
                            Just path -> ([Core.SetSearchPath effScope path], [])
                            Nothing ->
                                ( []
                                ,
                                    [ Core.ScopedBarrier
                                        Core.GlobalScope
                                        Core.UnknownSessionEffectBarrier
                                        (Just "SET search_path with a non-name value")
                                    ]
                                )
                    k | k `elem` ["foreign_keys", "foreign_key_checks"] ->
                        case listToMaybe values >>= exprBool of
                            Nothing ->
                                ( []
                                ,
                                    [ Core.ScopedBarrier
                                        Core.GlobalScope
                                        Core.UnknownSessionEffectBarrier
                                        (Just "SET foreign_keys with a non-boolean value")
                                    ]
                                )
                            Just b -> case systemVarScope name setScope of
                                Nothing ->
                                    ( []
                                    ,
                                        [ Core.ScopedBarrier
                                            Core.GlobalScope
                                            Core.UnknownSessionEffectBarrier
                                            (Just "SET foreign_keys with an unmodeled scope or qualifier")
                                        ]
                                    )
                                Just effScope -> ([Core.SetForeignKeyChecks effScope b], [])
                    "autocommit" ->
                        case listToMaybe values >>= exprBool of
                            Nothing ->
                                ( []
                                ,
                                    [ Core.ScopedBarrier
                                        Core.GlobalScope
                                        Core.UnknownSessionEffectBarrier
                                        (Just "SET autocommit with a non-boolean value")
                                    ]
                                )
                            Just b -> case systemVarScope name setScope of
                                Nothing ->
                                    ( []
                                    ,
                                        [ Core.ScopedBarrier
                                            Core.GlobalScope
                                            Core.UnknownSessionEffectBarrier
                                            (Just "SET autocommit with an unmodeled scope or qualifier")
                                        ]
                                    )
                                Just effScope -> ([Core.SetAutocommit effScope b], [])
                    _ ->
                        let val = maybe Core.SessionDefault (exprValue ctx) (listToMaybe values)
                         in ([Core.SetVariable scope name val], [])

parseSysVarName :: AnalysisContext -> ByteString -> Maybe Core.Name
parseSysVarName ctx raw = do
    parts <- splitIdentifierList '.' (TE.decodeUtf8 raw)
    let n = length parts
        foldOne idx part =
            let cls = if idx + 1 < n then IdentRelation else IdentOther
                trimmed = T.strip part
             in if T.isPrefixOf "`" trimmed && T.isSuffixOf "`" trimmed && T.length trimmed >= 2
                    then Just (foldIdentText (ctxIdentRules ctx) cls True (T.drop 1 (T.dropEnd 1 trimmed)))
                    else Just (foldIdentText (ctxIdentRules ctx) cls False trimmed)
    Core.Name <$> zipWithM foldOne [0 ..] parts

searchPathValue :: AnalysisContext -> [AST.Expr] -> Maybe Core.SearchPathValue
searchPathValue ctx = \case
    [AST.ExprDefault] -> Just Core.SearchPathDefault
    [AST.ExprColumn (L _ (AST.Name [L _ (AST.NamePart raw False)]))]
        | T.toLower (TE.decodeUtf8 raw) == "default" ->
            Just Core.SearchPathDefault
    values -> Core.SearchPathExplicit . concat <$> traverse (searchPathEntries ctx) values

searchPathEntries :: AnalysisContext -> AST.Expr -> Maybe [Core.SearchPathEntry]
searchPathEntries ctx = \case
    AST.ExprColumn (L _ (AST.Name [L _ part]))
        | AST.namePartQuoted part && T.null (unquoteNamePart part) -> Nothing
        | unquoteNamePart part == "$user" -> Just [Core.SearchPathCurrentUser]
        | otherwise ->
            Just [Core.SearchPathName (Core.Name [canonicalIdent ctx IdentRelation part])]
    AST.ExprColumn (L _ astName) ->
        (: []) . Core.SearchPathName <$> nameFromAST ctx IdentRelation astName
    AST.ExprParam AST.ParamPositional raw
        | raw == "$user" -> Just [Core.SearchPathCurrentUser]
    AST.ExprLiteral (AST.Literal LitString raw) ->
        unquoteLiteral (TE.decodeUtf8 raw) >>= literalSearchPathEntries ctx
    _ -> Nothing

literalSearchPathEntries :: AnalysisContext -> Text -> Maybe [Core.SearchPathEntry]
literalSearchPathEntries ctx value = do
    parts <- splitIdentifierList ',' value
    traverse parseOne (filter (not . T.null . T.strip) parts)
    where
        parseOne part = do
            (nameText, quoted) <- literalIdentifierPart part
            if nameText == "$user"
                then Just Core.SearchPathCurrentUser
                else
                    Just
                        (Core.SearchPathName (Core.Name [foldIdentText (ctxIdentRules ctx) IdentRelation quoted nameText]))
