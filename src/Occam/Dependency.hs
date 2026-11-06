module Occam.Dependency (
    ObjectName,
    SQLDialect (..),
    defaultSQLDialect,
    ObjectKind (..),
    DependencyType (..),
    ObjectAction (..),
    DependencyGraph (..),
    emptyGraph,
    isBarrierAction,
    extractActions,
    extractActionsWithDialect,
    buildGraph,
    analyzeStatements,
    analyzeStatementsWithDialect,
) where

import Data.ByteString (ByteString)
import Data.Char (isSpace)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Occam.Core (Statement (..), StatementId (..))
import Occam.SQLLexer (SQLToken (..), lexSQL)
import Occam.TreeSitter (
    StatementUnit (..),
    TSNode,
    collectDescendants,
    nodeNamedChildren,
    nodeText,
    nodeType,
 )

type ObjectName = Text

data SQLDialect
    = GenericSQL
    | PostgreSQL
    | MySQL
    | SQLite
    deriving (Eq, Ord, Show)

defaultSQLDialect :: SQLDialect
defaultSQLDialect = GenericSQL

data ObjectKind
    = TableObject
    | TemporaryTableObject
    | ColumnObject
    | IndexObject
    | ViewObject
    | TriggerObject
    | BarrierObject
    deriving (Eq, Ord, Show)

data DependencyType
    = DefDep
    | UseDep
    | ModDep
    | DropDep
    | RenameFromDep
    | RenameToDep
    | BarrierDep
    deriving (Eq, Ord, Show)

data ObjectAction = ObjectAction
    { actionKind :: !ObjectKind
    , actionType :: !DependencyType
    , actionName :: !ObjectName
    }
    deriving (Eq, Ord, Show)

data DependencyGraph = DependencyGraph
    { graphStmts :: !(Set StatementId)
    , graphActions :: !(Map StatementId [ObjectAction])
    , graphEdges :: !(Map StatementId (Set StatementId))
    , graphBarrierActions :: !Int
    , graphBarrierStatements :: !Int
    , graphBarrierEdges :: !Int
    , graphEdgeCount :: !Int
    }
    deriving (Eq, Show)

emptyGraph :: DependencyGraph
emptyGraph =
    DependencyGraph
        { graphStmts = Set.empty
        , graphActions = Map.empty
        , graphEdges = Map.empty
        , graphBarrierActions = 0
        , graphBarrierStatements = 0
        , graphBarrierEdges = 0
        , graphEdgeCount = 0
        }

isBarrierAction :: ObjectAction -> Bool
isBarrierAction act =
    actionKind act == BarrierObject || actionType act == BarrierDep

-- PostgreSQL preserves quoted case; SQLite is case-insensitive, and MySQL names
-- are folded case-insensitively as a conservative lower_case_table_names assumption.
normalizeName :: SQLDialect -> ByteString -> Text
normalizeName dialect = canonicalName dialect . T.strip . TE.decodeUtf8Lenient

canonicalName :: SQLDialect -> Text -> Text
canonicalName dialect =
    T.intercalate "." . map normalizePart . splitIdentifier
    where
        normalizePart (quoted, part) =
            let normalized =
                    if dialect == PostgreSQL && quoted
                        then part
                        else T.toLower part
             in escapePart normalized

splitIdentifier :: Text -> [(Bool, Text)]
splitIdentifier = go . T.unpack
    where
        go input =
            case dropWhile isSpace input of
                [] -> []
                opener : rest
                    | Just closer <- closingQuote opener ->
                        let (part, suffix) = quoted closer rest
                         in (True, T.pack part) : go (afterDot suffix)
                    | otherwise ->
                        let (part, suffix) = break (== '.') input
                         in (False, T.strip (T.pack part)) : go (afterDot suffix)

        quoted _ [] = ([], [])
        quoted closer (c : cs)
            | c == closer =
                case cs of
                    next : rest | next == closer -> let (part, suffix) = quoted closer rest in (closer : part, suffix)
                    _ -> ([], cs)
            | otherwise =
                let (part, suffix) = quoted closer cs
                 in (c : part, suffix)

        afterDot suffix =
            case dropWhile isSpace suffix of
                '.' : rest -> rest
                rest -> rest

closingQuote :: Char -> Maybe Char
closingQuote '"' = Just '"'
closingQuote '`' = Just '`'
closingQuote '[' = Just ']'
closingQuote _ = Nothing

escapePart :: Text -> Text
escapePart = T.concatMap escape
    where
        escape c
            | c == '.' || c == '\\' = T.pack ['\\', c]
            | otherwise = T.singleton c

-- Canonical parts retain their escapes and can be joined without re-encoding.
canonicalParts :: Text -> [Text]
canonicalParts = go . T.unpack
    where
        go chars =
            let (part, rest) = segment [] chars
             in T.pack (reverse part) : case rest of
                    [] -> []
                    _ : more -> go more

        segment acc [] = (acc, [])
        segment acc ('\\' : c : rest) = segment (c : '\\' : acc) rest
        segment acc ['\\'] = ('\\' : acc, [])
        segment acc ('.' : rest) = (acc, '.' : rest)
        segment acc (c : rest) = segment (c : acc) rest

isQualifiedName :: Text -> Bool
isQualifiedName name = length (canonicalParts name) > 1

lastNamePart :: Text -> Text
lastNamePart name =
    case canonicalParts name of
        [] -> name
        parts -> last parts

columnName :: ObjectName -> ObjectName -> ObjectName
columnName table column = table <> "\x1f" <> column

-- ----------------------------------------------------------------------------
-- CST Pattern Matching & Action Extraction
-- ----------------------------------------------------------------------------

hasDescendantType :: Text -> TSNode -> IO Bool
hasDescendantType targetType node = do
    matches <- collectDescendants (== targetType) node
    pure (not (null matches))

extractActions :: StatementUnit -> ByteString -> IO [ObjectAction]
extractActions = extractActionsWithDialect defaultSQLDialect

extractActionsWithDialect :: SQLDialect -> StatementUnit -> ByteString -> IO [ObjectAction]
extractActionsWithDialect dialect statementUnit src = do
    control <- transactionControlForStatement dialect statementUnit
    case control of
        Just command -> pure [ObjectAction BarrierObject BarrierDep (transactionActionName command)]
        Nothing ->
            case unitNode statementUnit of
                Nothing -> pure [ObjectAction BarrierObject BarrierDep "__unknown_stmt_barrier__"]
                Just stmtNode -> do
                    rootType <- nodeType stmtNode
                    hasError <- hasDescendantType "ERROR" stmtNode
                    if hasError || rootType == "ERROR"
                        then pure [ObjectAction BarrierObject BarrierDep "__syntax_error_barrier__"]
                        else
                            if not (supportedStatement rootType)
                                then pure [ObjectAction BarrierObject BarrierDep "__unknown_stmt_barrier__"]
                                else do
                                    actions <- inspectChild dialect Set.empty src stmtNode
                                    pure $
                                        if null actions
                                            then []
                                            else Set.toList (Set.fromList actions)

supportedStatement :: Text -> Bool
supportedStatement t =
    t
        `elem` [ "statement"
               , "create_table"
               , "create_view"
               , "create_materialized_view"
               , "create_index"
               , "create_trigger"
               , "drop_table"
               , "drop_view"
               , "drop_materialized_view"
               , "drop_index"
               , "drop_trigger"
               , "alter_table"
               , "_select_statement"
               , "insert"
               , "replace"
               , "update"
               , "delete"
               , "_truncate_statement"
               , "truncate"
               , "select"
               , "from"
               , "transaction"
               , "set"
               , "set_statement"
               , "reset_statement"
               ]

inspectStatement :: SQLDialect -> Set ObjectName -> ByteString -> TSNode -> IO [ObjectAction]
inspectStatement dialect outerCTEs src node = do
    children <- scopedQueryChildren dialect outerCTEs src node
    let childTypes = [t | (t, _, _) <- children]
    if "keyword_truncate" `elem` childTypes
        then truncateActions dialect outerCTEs src node
        else
            if null children
                then pure [unknownStatement]
                else concat <$> mapM inspectStatementChild children
    where
        unknownStatement = ObjectAction BarrierObject BarrierDep "__unknown_stmt_barrier__"

        inspectStatementChild (t, child, env)
            | (supportedStatement t && t /= "statement") || t == "cte" =
                inspectChild dialect env src child
            | t `elem` statementWrappers = do
                nested <- scopedQueryChildren dialect env src child
                if null nested
                    then pure [unknownStatement]
                    else concat <$> mapM inspectStatementChild nested
            | t `elem` ["keyword_with", "keyword_recursive"] = pure []
            | otherwise = do
                nested <- nodeNamedChildren child
                nestedActions <- concat <$> mapM (inspectChild dialect env src) nested
                pure (unknownStatement : nestedActions)

        statementWrappers =
            [ "_ddl_statement"
            , "_create_statement"
            , "_alter_statement"
            , "_drop_statement"
            , "_insert_statement"
            , "_update_statement"
            , "_delete_statement"
            , "_truncate_statement"
            , "_select_statement"
            , "_select_body"
            , "_subquery_body"
            , "_dml_write"
            , "_dml_read"
            ]

inspectChild :: SQLDialect -> Set ObjectName -> ByteString -> TSNode -> IO [ObjectAction]
inspectChild dialect outerCTEs src node = do
    t <- nodeType node
    case t of
        "statement" -> inspectStatement dialect outerCTEs src node
        "_select_statement" -> queryActions dialect outerCTEs src node
        "create_table" -> createTableActions dialect src node
        "create_view" -> createViewActions dialect src node
        "create_materialized_view" -> createViewActions dialect src node
        "create_index" -> createIndexActions dialect src node
        "create_trigger" -> createTriggerActions dialect src node
        "drop_table" -> dropActions dialect TableObject src node
        "drop_view" -> dropActions dialect ViewObject src node
        "drop_materialized_view" -> dropActions dialect ViewObject src node
        "drop_index" -> dropActions dialect IndexObject src node
        "drop_trigger" -> dropActions dialect TriggerObject src node
        "alter_table" -> alterTableActions dialect src node
        "insert" -> dmlActions dialect outerCTEs src node
        "replace" -> dmlActions dialect outerCTEs src node
        "update" -> dmlActions dialect outerCTEs src node
        "delete" -> dmlActions dialect outerCTEs src node
        "_truncate_statement" -> truncateActions dialect outerCTEs src node
        "truncate" -> truncateActions dialect outerCTEs src node
        "select" -> queryActions dialect outerCTEs src node
        "from" -> queryActions dialect outerCTEs src node
        "transaction" -> pure [ObjectAction BarrierObject BarrierDep "__tx_barrier__"]
        "set" -> pure [sessionBarrier]
        "set_statement" -> pure [sessionBarrier]
        "reset_statement" -> pure [sessionBarrier]
        _ -> do
            children <- nodeNamedChildren node
            concat <$> mapM (inspectChild dialect outerCTEs src) children
    where
        sessionBarrier = ObjectAction BarrierObject BarrierDep "__session_barrier__"
truncateActions :: SQLDialect -> Set ObjectName -> ByteString -> TSNode -> IO [ObjectAction]
truncateActions dialect outerCTEs src node = do
    tables <- findTableReferences dialect outerCTEs src node
    pure $
        case tables of
            [] -> [ObjectAction BarrierObject BarrierDep "__unknown_truncate_barrier__"]
            _ -> [ObjectAction TableObject ModDep table | table <- tables]

createTableActions :: SQLDialect -> ByteString -> TSNode -> IO [ObjectAction]
createTableActions dialect src node = do
    target <- findDirectTarget dialect src node
    case target of
        Nothing -> pure [ObjectAction BarrierObject BarrierDep "__unknown_ddl_barrier__"]
        Just (table, targetNode) -> do
            references <- findTableReferencesExcept dialect Set.empty [targetNode] src node
            temporary <-
                (||)
                    <$> hasDescendantType "keyword_temp" node
                    <*> hasDescendantType "keyword_temporary" node
            definitions <- findColumnDefinitions dialect table src node
            let temporarySchema =
                    dialect `elem` [SQLite, PostgreSQL] && isQualifiedName table && isTemporarySchema table
                tableKind = if temporary || temporarySchema then TemporaryTableObject else TableObject
            pure $
                ObjectAction tableKind DefDep table
                    : [ObjectAction TableObject UseDep ref | ref <- references]
                    ++ definitions

createViewActions :: SQLDialect -> ByteString -> TSNode -> IO [ObjectAction]
createViewActions dialect src node = do
    target <- findDirectTarget dialect src node
    case target of
        Nothing -> pure [ObjectAction BarrierObject BarrierDep "__unknown_view_barrier__"]
        Just (view, targetNode) -> do
            references <- findTableReferencesExcept dialect Set.empty [targetNode] src node
            columns <- columnUseActions dialect references src node
            pure $
                ObjectAction ViewObject DefDep view
                    : [ObjectAction TableObject UseDep ref | ref <- references]
                    ++ columns

createIndexActions :: SQLDialect -> ByteString -> TSNode -> IO [ObjectAction]
createIndexActions dialect src node = do
    children <- nodeNamedChildren node
    typed <- mapM (\child -> (,child) <$> nodeType child) children
    let targets =
            [ child
            | (t, child) <- takeWhile ((/= "keyword_on") . fst) typed
            , t `elem` ["identifier", "object_reference"]
            ]
    case reverse targets of
        [] -> pure [ObjectAction BarrierObject BarrierDep "__unknown_index_barrier__"]
        targetNode : _ -> do
            index <- normalizeName dialect <$> nodeText src targetNode
            references <- findTableReferencesExcept dialect Set.empty [targetNode] src node
            columns <- columnUseActions dialect references src node
            pure $
                ObjectAction IndexObject DefDep index
                    : [ObjectAction TableObject UseDep ref | ref <- references]
                    ++ columns

createTriggerActions :: SQLDialect -> ByteString -> TSNode -> IO [ObjectAction]
createTriggerActions dialect src node = do
    target <- findDirectTarget dialect src node
    case target of
        Nothing -> pure [ObjectAction BarrierObject BarrierDep "__unknown_trigger_barrier__"]
        Just (trigger, targetNode) -> do
            references <- findTableReferencesExcept dialect Set.empty [targetNode] src node
            pure $
                ObjectAction TriggerObject DefDep trigger
                    : [ObjectAction TableObject UseDep ref | ref <- references]
                    ++ [ObjectAction BarrierObject BarrierDep "__trigger_semantics_barrier__"]

dropActions :: SQLDialect -> ObjectKind -> ByteString -> TSNode -> IO [ObjectAction]
dropActions dialect kind src node = do
    tables <- findTableReferences dialect Set.empty src node
    target <- findDirectTarget dialect src node
    temporary <-
        if kind == TableObject
            then
                (||)
                    <$> hasDescendantType "keyword_temp" node
                    <*> hasDescendantType "keyword_temporary" node
            else pure False
    let actionKind' = if temporary then TemporaryTableObject else kind
        names =
            if null tables
                then maybe [] (pure . fst) target
                else tables
    pure $
        if null names
            then [ObjectAction BarrierObject BarrierDep "__unknown_drop_barrier__"]
            else [ObjectAction actionKind' DropDep name | name <- names]

alterTableActions :: SQLDialect -> ByteString -> TSNode -> IO [ObjectAction]
alterTableActions dialect src node = do
    target <- findDirectTarget dialect src node
    renameNodes <- collectDescendants (== "rename_object") node
    case (target, renameNodes) of
        (Just (oldName, _), renameNode : _) -> do
            newName <- lastIdentifierName dialect src renameNode
            case newName of
                Just rawNew -> do
                    let newName' = qualifyRename oldName rawNew
                    pure
                        [ ObjectAction TableObject RenameFromDep oldName
                        , ObjectAction TableObject RenameToDep newName'
                        ]
                Nothing -> pure [ObjectAction TableObject ModDep oldName, unknownAlter]
        (Just (table, _), []) -> do
            added <- collectDescendants (== "column_definition") node
            addActions <- fmap concat (mapM (columnDefinitionAction dialect table src) added)
            dropped <- collectDescendants (== "drop_column") node
            dropActions' <- fmap concat (mapM (columnDropAction dialect table src) dropped)
            renamed <- collectDescendants (== "rename_column") node
            renameActions <- fmap concat (mapM (columnRenameActions dialect table src) renamed)
            pure $
                ObjectAction TableObject ModDep table
                    : addActions
                    ++ dropActions'
                    ++ renameActions
        _ -> pure [unknownAlter]
    where
        unknownAlter = ObjectAction BarrierObject BarrierDep "__unknown_alter_barrier__"

qualifyRename :: ObjectName -> ObjectName -> ObjectName
qualifyRename oldName newName
    | isQualifiedName oldName && not (isQualifiedName newName) =
        T.intercalate "." (init (canonicalParts oldName) ++ [newName])
    | otherwise = newName

columnDefinitionAction :: SQLDialect -> ObjectName -> ByteString -> TSNode -> IO [ObjectAction]
columnDefinitionAction dialect table src node = do
    name <- firstIdentifierName dialect src node
    pure [ObjectAction ColumnObject DefDep (columnName table col) | col <- maybeToList name]

columnDropAction :: SQLDialect -> ObjectName -> ByteString -> TSNode -> IO [ObjectAction]
columnDropAction dialect table src node = do
    name <- firstIdentifierName dialect src node
    pure [ObjectAction ColumnObject DropDep (columnName table col) | col <- maybeToList name]

columnRenameActions :: SQLDialect -> ObjectName -> ByteString -> TSNode -> IO [ObjectAction]
columnRenameActions dialect table src node = do
    names <- identifierNames dialect src node
    pure $ case names of
        oldName : newName : _ ->
            [ ObjectAction ColumnObject DropDep (columnName table oldName)
            , ObjectAction ColumnObject DefDep (columnName table newName)
            ]
        _ -> []

findColumnDefinitions :: SQLDialect -> ObjectName -> ByteString -> TSNode -> IO [ObjectAction]
findColumnDefinitions dialect table src node = do
    definitions <- collectDescendants (== "column_definition") node
    concat <$> mapM (columnDefinitionAction dialect table src) definitions

findDirectTarget :: SQLDialect -> ByteString -> TSNode -> IO (Maybe (ObjectName, TSNode))
findDirectTarget dialect src node = do
    children <- nodeNamedChildren node
    findTarget children
    where
        findTarget [] = pure Nothing
        findTarget (child : rest) = do
            t <- nodeType child
            if t == "object_reference"
                then targetFrom child
                else
                    if t == "relation"
                        then do
                            refs <- collectDescendants (== "object_reference") child
                            case refs of
                                ref : _ -> targetFrom ref
                                [] -> targetFrom child
                        else findTarget rest

        targetFrom candidate = do
            name <- normalizeName dialect <$> nodeText src candidate
            if T.null name
                then pure Nothing
                else pure (Just (name, candidate))

firstIdentifierName :: SQLDialect -> ByteString -> TSNode -> IO (Maybe ObjectName)
firstIdentifierName dialect src node = do
    names <- identifierNames dialect src node
    pure $ case names of
        name : _ -> Just name
        [] -> Nothing

lastIdentifierName :: SQLDialect -> ByteString -> TSNode -> IO (Maybe ObjectName)
lastIdentifierName dialect src node = do
    names <- identifierNames dialect src node
    pure $ case names of
        [] -> Nothing
        _ -> Just (last names)

identifierNames :: SQLDialect -> ByteString -> TSNode -> IO [ObjectName]
identifierNames dialect src node = do
    ids <- collectDescendants isIdentifierNode node
    names <- mapM (fmap (normalizeName dialect) . nodeText src) ids
    pure (filter (not . T.null) names)
    where
        isIdentifierNode t =
            t `elem` ["identifier", "object_id", "object_reference", "field"]

dmlActions :: SQLDialect -> Set ObjectName -> ByteString -> TSNode -> IO [ObjectAction]
dmlActions dialect outerCTEs src node = do
    t <- nodeType node
    target <- findDirectTarget dialect src node
    case target of
        Nothing -> pure [ObjectAction BarrierObject BarrierDep "__unknown_dml_barrier__"]
        Just (targetName, targetNode) -> do
            sources <- findTableReferencesExcept dialect outerCTEs [targetNode] src node
            columns <-
                columnUseActions
                    dialect
                    (if t `elem` ["update", "delete"] then targetName : sources else sources)
                    src
                    node
            columnWrites <-
                if t `elem` ["update", "insert", "replace"]
                    then updateColumnActions dialect targetName src node
                    else pure []
            insertedColumns <-
                if t `elem` ["insert", "replace"]
                    then insertColumnActions dialect targetName src node
                    else pure []
            pure $
                ObjectAction TableObject ModDep targetName
                    : [ObjectAction TableObject UseDep source | source <- sources]
                    ++ columns
                    ++ columnWrites
                    ++ insertedColumns

insertColumnActions :: SQLDialect -> ObjectName -> ByteString -> TSNode -> IO [ObjectAction]
insertColumnActions dialect table src node = do
    children <- nodeNamedChildren node
    typed <- mapM (\child -> (,child) <$> nodeType child) children
    let targetLists =
            [ child
            | (t, child) <- takeWhile ((`notElem` ["keyword_values", "select", "from"]) . fst) typed
            , t == "list"
            ]
    names <- concat <$> mapM (identifierNames dialect src) targetLists
    pure [ObjectAction ColumnObject ModDep (columnName table (lastNamePart name)) | name <- names]

updateColumnActions :: SQLDialect -> ObjectName -> ByteString -> TSNode -> IO [ObjectAction]
updateColumnActions dialect table src node = do
    assignments <- collectDescendants (== "assignment") node
    names <- mapM assignmentTarget assignments
    pure
        [ ObjectAction ColumnObject ModDep (columnName table (lastNamePart name))
        | name <- Set.toList (Set.fromList (catMaybes names))
        ]
    where
        assignmentTarget assignment = do
            children <- nodeNamedChildren assignment
            case children of
                target : _ -> firstIdentifierName dialect src target
                [] -> pure Nothing

queryActions :: SQLDialect -> Set ObjectName -> ByteString -> TSNode -> IO [ObjectAction]
queryActions dialect outerCTEs src node = do
    tables <- findTableReferences dialect outerCTEs src node
    columns <- columnUseActions dialect tables src node
    pure ([ObjectAction TableObject UseDep table | table <- tables] ++ columns)

columnUseActions :: SQLDialect -> [ObjectName] -> ByteString -> TSNode -> IO [ObjectAction]
columnUseActions dialect tables src node = do
    fields <- collectDescendants isField node
    fieldNames <- mapM (fmap (normalizeName dialect) . nodeText src) fields
    let actions =
            [ ObjectAction ColumnObject UseDep (columnName table column)
            | fieldName <- Set.toList (Set.fromList (filter (not . T.null) fieldNames))
            , let parts = canonicalParts fieldName
            , not (null parts)
            , let column = last parts
            , let qualifier = T.intercalate "." (init parts)
            , table <- columnTables qualifier tables
            ]
    pure actions
    where
        isField t = t == "field" || t == "_qualified_field"

        columnTables qualifier candidates
            | T.null qualifier = candidates
            | otherwise =
                let exact =
                        [ table
                        | table <- candidates
                        , table == qualifier || lastNamePart table == lastNamePart qualifier
                        ]
                 in if null exact then candidates else exact

findTableReferences :: SQLDialect -> Set ObjectName -> ByteString -> TSNode -> IO [ObjectName]
findTableReferences dialect outerCTEs =
    findTableReferencesExcept dialect outerCTEs []

findTableReferencesExcept ::
    SQLDialect ->
    Set ObjectName ->
    [TSNode] ->
    ByteString ->
    TSNode ->
    IO [ObjectName]
findTableReferencesExcept dialect outerCTEs excluded src node = do
    refs <- collectRefs outerCTEs node
    names <- mapM (fmap (normalizeName dialect) . nodeText src) refs
    pure
        ( Set.toList
            ( Set.fromList
                [ name
                | (ref, name) <- zip refs names
                , ref `notElem` excluded
                , not (T.null name)
                ]
            )
        )
    where
        collectRefs env current = do
            t <- nodeType current
            if t == "invocation"
                then pure []
                else do
                    children <- scopedQueryChildren dialect env src current
                    let visibleChildren = if t == "cte" then drop 1 children else children
                    childRefs <- concat <$> mapM (\(_, child, childEnv) -> collectRefs childEnv child) visibleChildren
                    hasObjectRef <- hasDescendantType "object_reference" current
                    let isReference = t == "object_reference" || (t == "relation" && not hasObjectRef)
                    if not isReference
                        then pure childRefs
                        else do
                            raw <- nodeText src current
                            let name = normalizeName dialect raw
                            pure $
                                if isCTEReference env name
                                    then childRefs
                                    else current : childRefs

        isCTEReference env name =
            not (isQualifiedName name) && Set.member (lastNamePart name) env

scopedQueryChildren ::
    SQLDialect -> Set ObjectName -> ByteString -> TSNode -> IO [(Text, TSNode, Set ObjectName)]
scopedQueryChildren dialect outerCTEs src node = do
    children <- nodeNamedChildren node
    typed <- mapM identify children
    let localCTEs = Set.fromList (mapMaybe (\(_, _, alias) -> alias) typed)
        visible = Set.union outerCTEs localCTEs
        recursive = any (\(t, _, _) -> t == "keyword_recursive") typed
        scope _ [] = []
        scope prior ((t, child, alias) : rest) =
            let env = if t == "cte" && not recursive then prior else visible
                next = maybe prior (`Set.insert` prior) alias
             in (t, child, env) : scope next rest
    pure (scope outerCTEs typed)
    where
        identify child = do
            t <- nodeType child
            alias <-
                if t == "cte"
                    then do
                        parts <- nodeNamedChildren child
                        case parts of
                            nameNode : _ -> do
                                name <- normalizeName dialect <$> nodeText src nameNode
                                pure (if T.null name then Nothing else Just name)
                            [] -> pure Nothing
                    else pure Nothing
            pure (t, child, alias)

transactionControlForStatement :: SQLDialect -> StatementUnit -> IO (Maybe TxCommand)
transactionControlForStatement dialect statementUnit =
    pure $
        parseTransactionControl
            dialect
            (stmtRaw (unitStatement statementUnit))

parseTransactionControl :: SQLDialect -> ByteString -> Maybe TxCommand
parseTransactionControl dialect sql =
    case lexSQL sql of
        WordToken "begin" : _ -> Just TxBegin
        WordToken "start" : WordToken "transaction" : _ -> Just TxBegin
        WordToken "commit" : _ -> Just TxCommit
        WordToken "end" : _ -> Just TxCommit
        WordToken "rollback" : WordToken "to" : rest ->
            TxRollbackTo <$> savepointName dialect (dropSavepointKeyword rest)
        WordToken "rollback" : WordToken "work" : WordToken "to" : rest ->
            TxRollbackTo <$> savepointName dialect (dropSavepointKeyword rest)
        WordToken "rollback" : WordToken "transaction" : WordToken "to" : rest ->
            TxRollbackTo <$> savepointName dialect (dropSavepointKeyword rest)
        WordToken "rollback" : _ -> Just TxRollback
        WordToken "abort" : _ -> Just TxRollback
        WordToken "savepoint" : rest -> TxSavepoint <$> savepointName dialect rest
        WordToken "release" : rest -> TxRelease <$> savepointName dialect (dropSavepointKeyword rest)
        _ -> Nothing
    where
        dropSavepointKeyword (WordToken "savepoint" : rest) = rest
        dropSavepointKeyword rest = rest

savepointName :: SQLDialect -> [SQLToken] -> Maybe ObjectName
savepointName dialect tokens =
    case tokens of
        WordToken name : _ -> Just (canonicalName dialect name)
        IdentifierToken name : _ -> Just (canonicalPart dialect True name)
        _ -> Nothing

canonicalPart :: SQLDialect -> Bool -> Text -> Text
canonicalPart dialect isQuoted part =
    escapePart
        ( if dialect == PostgreSQL && isQuoted
            then part
            else T.toLower part
        )

data TxCommand
    = TxBegin
    | TxCommit
    | TxRollback
    | TxSavepoint !ObjectName
    | TxRollbackTo !ObjectName
    | TxRelease !ObjectName
    deriving (Eq, Show)

transactionActionName :: TxCommand -> ObjectName
transactionActionName TxBegin = "__tx_begin__"
transactionActionName TxCommit = "__tx_commit__"
transactionActionName TxRollback = "__tx_rollback__"
transactionActionName (TxSavepoint name) = "__tx_savepoint__\x1e" <> name
transactionActionName (TxRollbackTo name) = "__tx_rollback_to__\x1e" <> name
transactionActionName (TxRelease name) = "__tx_release__\x1e" <> name

-- ----------------------------------------------------------------------------
-- Dependency DAG Construction
-- ----------------------------------------------------------------------------

{- | Builds dependencies between definitions, reads, writes, and conservative
   sequence barriers. Definitions replace the current object generation.
-}
buildGraph :: [(StatementId, [ObjectAction])] -> DependencyGraph
buildGraph stmtActions =
    let (_state, edges, barrierEdges, barrierStatements, barrierActions) =
            foldl' processStmt (initialState, Map.empty, 0, 0, 0) stmtActions
     in DependencyGraph
            { graphStmts = Set.fromList (map fst stmtActions)
            , graphActions = Map.fromList stmtActions
            , graphEdges = edges
            , graphBarrierActions = barrierActions
            , graphBarrierStatements = barrierStatements
            , graphBarrierEdges = barrierEdges
            , graphEdgeCount = sum (map Set.size (Map.elems edges))
            }

data ObjectKey
    = ObjectKey !ObjectKind !ObjectName
    | ColumnKey !ObjectKey !ObjectName
    deriving (Eq, Ord, Show)

data ObjectState = ObjectState
    { objectDefinition :: !(Maybe StatementId)
    , objectLastWrite :: !(Maybe StatementId)
    , objectSources :: !(Set (Either ObjectName ObjectKey))
    , objectDropped :: !Bool
    }
    deriving (Eq, Show)

data Catalog = Catalog
    { catalogObjects :: !(Map ObjectKey ObjectState)
    , catalogRelations :: !(Map ObjectKey (Set ObjectKey))
    }
    deriving (Eq, Show)

data TxFrame = TxFrame
    { txStart :: !Catalog
    , txSavepoints :: ![(ObjectName, Catalog)]
    , txImplicitSavepoint :: !Bool
    }
    deriving (Eq, Show)

data ScanState = ScanState
    { scanCatalog :: !Catalog
    , scanTransactions :: ![TxFrame]
    , scanLastBarrier :: !(Maybe StatementId)
    , scanPriorStatements :: !(Set StatementId)
    }

initialState :: ScanState
initialState =
    ScanState
        { scanCatalog = Catalog Map.empty Map.empty
        , scanTransactions = []
        , scanLastBarrier = Nothing
        , scanPriorStatements = Set.empty
        }

processStmt ::
    (ScanState, Map StatementId (Set StatementId), Int, Int, Int) ->
    (StatementId, [ObjectAction]) ->
    (ScanState, Map StatementId (Set StatementId), Int, Int, Int)
processStmt (state, edgeMap, barrierEdgeCount, barrierStmtCount, barrierActionCount) (sid, actions) =
    let
        declaredBarriers = length (filter isBarrierAction actions)
        catalogBarrier = declaredBarriers == 0 && needsCatalogBarrier state actions
        hasBarrier = declaredBarriers > 0 || catalogBarrier
        barrierDeps =
            if hasBarrier
                then scanPriorStatements state
                else maybe Set.empty Set.singleton (scanLastBarrier state)
        (objectDeps, state') = processObjectActions sid actions state
        (controlDeps, state'') = applyTransactionControl sid actions state'
        allDeps = Set.delete sid (Set.unions [barrierDeps, objectDeps, controlDeps])
        edgeMap' = Map.insert sid allDeps edgeMap
        state''' =
            state''
                { scanLastBarrier = if hasBarrier then Just sid else scanLastBarrier state''
                , scanPriorStatements = Set.insert sid (scanPriorStatements state'')
                }
        newBarrierEdges = Set.size (Set.difference barrierDeps (Set.union objectDeps controlDeps))
        actionBarriers = declaredBarriers + fromEnum catalogBarrier
     in
        ( state'''
        , edgeMap'
        , barrierEdgeCount + newBarrierEdges
        , barrierStmtCount + if hasBarrier then 1 else 0
        , barrierActionCount + actionBarriers
        )

needsCatalogBarrier :: ScanState -> [ObjectAction] -> Bool
needsCatalogBarrier state = any uncertain
    where
        uncertain action
            | actionKind action /= TableObject = False
            | actionType action == ModDep =
                any ((== ViewObject) . objectKind) (liveMatches UseDep action)
            | actionType action `elem` [DropDep, RenameFromDep] =
                case liveMatches (actionType action) action of
                    _ : _ : _ -> True
                    _ -> False
            | otherwise = False
        liveMatches dependency action =
            filter (isLiveKey (scanCatalog state)) $
                matchingKeys state TableObject dependency (actionName action)

processObjectActions :: StatementId -> [ObjectAction] -> ScanState -> (Set StatementId, ScanState)
processObjectActions sid actions state =
    let
        priorState = state
        nonRename = filter (\act -> actionType act `notElem` [RenameFromDep, RenameToDep]) actions
        (actionDeps, state') =
            foldl' (processAction sid priorState actions) (Set.empty, state) nonRename
        (renameDeps, state'') = processRenames sid priorState actions state'
     in
        (Set.union actionDeps renameDeps, state'')

processAction ::
    StatementId ->
    ScanState ->
    [ObjectAction] ->
    (Set StatementId, ScanState) ->
    ObjectAction ->
    (Set StatementId, ScanState)
processAction sid priorState stmtActions (deps, state) act =
    case actionType act of
        DefDep ->
            let catalog = scanCatalog state
                keys = definitionKeys catalog stmtActions act
                sources = viewSources (scanCatalog priorState) stmtActions (actionName act)
                newObject = ObjectState (Just sid) Nothing sources False
                catalog0 =
                    if actionKind act `elem` [TableObject, TemporaryTableObject]
                        then foldl' (prepareDefinition sid) catalog keys
                        else catalog
                catalog' =
                    catalog0
                        { catalogObjects =
                            foldl' (\objects key -> Map.insert key newObject objects) (catalogObjects catalog0) keys
                        }
                catalog'' =
                    if actionKind act `elem` [IndexObject, TriggerObject]
                        then foldl' (\current key -> bindRelations sid key stmtActions current) catalog' keys
                        else catalog'
             in (deps, state {scanCatalog = catalog''})
        UseDep ->
            let objectDeps = dependenciesForAction priorState act
                indexDeps =
                    if actionKind act /= TableObject
                        || any (\action -> actionKind action == IndexObject && actionType action == DefDep) stmtActions
                        then Set.empty
                        else boundIndexDependencies (scanCatalog priorState) (actionName act)
             in (Set.union deps (Set.union objectDeps indexDeps), state)
        ModDep ->
            let priorDeps = dependenciesForAction priorState act
                keys = actionKeys priorState act
                catalog' = writeObjects sid keys (scanCatalog state)
             in (Set.union deps priorDeps, state {scanCatalog = catalog'})
        DropDep ->
            let priorDeps = dependenciesForAction priorState act
                keys = actionKeys priorState act
                catalog' = dropObjects sid keys (scanCatalog state)
             in (Set.union deps priorDeps, state {scanCatalog = catalog'})
        BarrierDep -> (deps, state)
        RenameFromDep -> (deps, state)
        RenameToDep -> (deps, state)

processRenames ::
    StatementId ->
    ScanState ->
    [ObjectAction] ->
    ScanState ->
    (Set StatementId, ScanState)
processRenames sid priorState actions state =
    foldl' rename (Set.empty, state) (zip sources targets)
    where
        sources = [actionName a | a <- actions, actionKind a == TableObject, actionType a == RenameFromDep]
        targets = [actionName a | a <- actions, actionKind a == TableObject, actionType a == RenameToDep]

        rename (deps, current) (oldName, newName) =
            let catalog = scanCatalog priorState
                oldKeys = filter (isLiveKey catalog) (matchingKeys priorState TableObject RenameFromDep oldName)
                sourceKeys = if null oldKeys then [ObjectKey TableObject oldName] else oldKeys
                sourceDeps =
                    Set.unions
                        [ Set.union (stateDependencies priorState key) (boundDependencies catalog (const True) key)
                        | key <- sourceKeys
                        ]
                move currentCatalog sourceKey =
                    case sourceKey of
                        ObjectKey kind storedName ->
                            let targetKey = ObjectKey kind (qualifyRename storedName newName)
                                dropped = dropObjects sid [sourceKey] currentCatalog
                                cleanTarget = prepareDefinition sid dropped targetKey
                                withTarget =
                                    cleanTarget
                                        { catalogObjects =
                                            Map.insert targetKey (ObjectState (Just sid) Nothing Set.empty False) (catalogObjects cleanTarget)
                                        , catalogRelations =
                                            moveRelations sourceKey targetKey (catalogRelations catalog) (catalogRelations cleanTarget)
                                        }
                                withSources = updateViewSources sourceKey targetKey withTarget
                             in moveTableColumns sid sourceKey targetKey catalog withSources
                        ColumnKey _ _ -> currentCatalog
             in (Set.union deps sourceDeps, current {scanCatalog = foldl' move (scanCatalog current) sourceKeys})

dependenciesForAction :: ScanState -> ObjectAction -> Set StatementId
dependenciesForAction state act =
    let keys = matchingKeys state (actionKind act) (actionType act) (actionName act)
        catalog = scanCatalog state
        objectDeps = Set.unions [recursiveDependencies catalog Set.empty key | key <- keys]
        relationDeps =
            case (actionKind act, actionType act) of
                (TableObject, ModDep) -> boundObjectDependencies catalog (actionName act)
                _ -> Set.empty
     in Set.union objectDeps relationDeps

recursiveDependencies :: Catalog -> Set ObjectKey -> ObjectKey -> Set StatementId
recursiveDependencies catalog seen key
    | Set.member key seen = Set.empty
    | otherwise =
        case Map.lookup key (catalogObjects catalog) of
            Nothing -> Set.empty
            Just object ->
                let here = stateDependenciesInObject object
                    seen' = Set.insert key seen
                    sources =
                        if objectKind key == ViewObject
                            then
                                Set.unions
                                    [ Set.union
                                        (recursiveDependencies catalog seen' sourceKey)
                                        (boundDependencies catalog ((== IndexObject) . objectKind) sourceKey)
                                    | source <- Set.toList (objectSources object)
                                    , sourceKey <- either (matchingKeysInCatalog catalog TableObject UseDep) (: []) source
                                    ]
                            else Set.empty
                 in Set.union here sources

stateDependencies :: ScanState -> ObjectKey -> Set StatementId
stateDependencies state key =
    maybe Set.empty stateDependenciesInObject (Map.lookup key (catalogObjects (scanCatalog state)))

stateDependenciesInObject :: ObjectState -> Set StatementId
stateDependenciesInObject object =
    Set.fromList (catMaybes [objectDefinition object, objectLastWrite object])

matchingKeys :: ScanState -> ObjectKind -> DependencyType -> ObjectName -> [ObjectKey]
matchingKeys state = matchingKeysInCatalog (scanCatalog state)

isLiveKey :: Catalog -> ObjectKey -> Bool
isLiveKey catalog key =
    maybe False (not . objectDropped) (Map.lookup key (catalogObjects catalog))

definitionKeys :: Catalog -> [ObjectAction] -> ObjectAction -> [ObjectKey]
definitionKeys catalog actions action
    | actionKind action /= ColumnObject = [ObjectKey (actionKind action) (actionName action)]
    | T.null suffix = [ObjectKey ColumnObject (actionName action)]
    | otherwise = [ColumnKey owner (T.drop 1 suffix) | owner <- owners]
    where
        (table, suffix) = T.breakOn "\x1f" (actionName action)
        declared =
            [ ObjectKey (actionKind definition) table
            | definition <- actions
            , actionType definition == DefDep
            , actionKind definition `elem` [TableObject, TemporaryTableObject]
            , actionName definition == table
            ]
        resolved = filter (isLiveKey catalog) (matchingKeysInCatalog catalog TableObject UseDep table)
        owners
            | not (null declared) = declared
            | not (null resolved) = resolved
            | otherwise = [ObjectKey TableObject table]

actionKeys :: ScanState -> ObjectAction -> [ObjectKey]
actionKeys state action =
    case matchingKeys state (actionKind action) (actionType action) (actionName action) of
        [] -> definitionKeys (scanCatalog state) [] action
        keys -> keys

prepareDefinition :: StatementId -> Catalog -> ObjectKey -> Catalog
prepareDefinition sid catalog key =
    case key of
        ObjectKey kind name ->
            let alias = ObjectKey kind (lastNamePart name)
                retireAlias =
                    isQualifiedName name
                        && maybe False objectDropped (Map.lookup alias (catalogObjects catalog))
                retired =
                    if retireAlias
                        then
                            catalog
                                { catalogObjects = Map.delete alias (catalogObjects catalog)
                                , catalogRelations = Map.delete alias (catalogRelations catalog)
                                }
                        else catalog
                cleared =
                    if Map.member key (catalogObjects catalog)
                        then resetTableColumns sid key retired
                        else retired
             in removeRelationsForTable key cleared
        ColumnKey _ _ -> catalog

matchingKeysInCatalog :: Catalog -> ObjectKind -> DependencyType -> ObjectName -> [ObjectKey]
matchingKeysInCatalog catalog ColumnObject dependency name
    | not (T.null suffix) =
        [ key
        | owner <- matchingKeysInCatalog catalog TableObject dependency table
        , let key = ColumnKey owner (T.drop 1 suffix)
        , Map.member key (catalogObjects catalog)
        ]
    where
        (table, suffix) = T.breakOn "\x1f" name
matchingKeysInCatalog catalog kind dependency name =
    let available = Map.keys (catalogObjects catalog)
        acceptableKinds = case (kind, dependency) of
            (TableObject, UseDep) -> [TableObject, TemporaryTableObject, ViewObject]
            (TableObject, ModDep) -> [TableObject, TemporaryTableObject, ViewObject]
            (TableObject, _) -> [TableObject, TemporaryTableObject]
            _ -> [kind]
        matches =
            [ key
            | key@(ObjectKey keyKind objectName) <- available
            , keyKind `elem` acceptableKinds
            , not
                ( keyKind == TemporaryTableObject
                    && isQualifiedName name
                    && not (isTemporarySchema name)
                    && objectName /= name
                )
            , namesMayReferToEachOther name objectName
            ]
        tempMatches =
            [ key
            | key@(ObjectKey TemporaryTableObject _) <- matches
            , isLiveKey catalog key
            ]
        shouldPreferTemp =
            kind == TableObject
                && dependency `elem` [UseDep, ModDep, DropDep, RenameFromDep]
                && not (null tempMatches)
     in if shouldPreferTemp then tempMatches else matches

isTemporarySchema :: ObjectName -> Bool
isTemporarySchema name =
    case canonicalParts name of
        schema : _ -> schema == "temp" || schema == "pg_temp" || "pg_temp_" `T.isPrefixOf` schema
        [] -> False

-- Without a known search_path, an unqualified name can resolve in any schema.
namesMayReferToEachOther :: ObjectName -> ObjectName -> Bool
namesMayReferToEachOther requested stored
    | requested == stored = True
    | not (isQualifiedName requested) || not (isQualifiedName stored) =
        lastNamePart requested == lastNamePart stored
    | otherwise = False

objectKind :: ObjectKey -> ObjectKind
objectKind (ObjectKey kind _) = kind
objectKind (ColumnKey _ _) = ColumnObject

writeObjects :: StatementId -> [ObjectKey] -> Catalog -> Catalog
writeObjects sid keys catalog =
    catalog
        { catalogObjects =
            foldl' write (catalogObjects catalog) keys
        }
    where
        write objects key =
            let old = Map.findWithDefault (ObjectState Nothing Nothing Set.empty False) key objects
             in Map.insert key old {objectLastWrite = Just sid} objects

dropObjects :: StatementId -> [ObjectKey] -> Catalog -> Catalog
dropObjects sid keys catalog =
    foldl' resetDroppedTable catalog' keys
    where
        catalog' =
            catalog
                { catalogObjects = foldl' dropOne (catalogObjects catalog) keys
                , catalogRelations = foldl' removeObjectBindings (catalogRelations catalog) keys
                }

        dropOne objects key =
            Map.insert key (ObjectState (Just sid) Nothing Set.empty True) objects

        removeObjectBindings relations key =
            Map.mapMaybe
                (\bound -> let kept = Set.delete key bound in if Set.null kept then Nothing else Just kept)
                relations

        resetDroppedTable current key
            | objectKind key `elem` [TableObject, TemporaryTableObject] =
                removeRelationsForTable key (resetTableColumns sid key current)
            | otherwise = current

removeRelationsForTable :: ObjectKey -> Catalog -> Catalog
removeRelationsForTable table catalog =
    catalog
        { catalogRelations = Map.delete table (catalogRelations catalog)
        }

resetTableColumns :: StatementId -> ObjectKey -> Catalog -> Catalog
resetTableColumns sid table catalog =
    catalog
        { catalogObjects =
            Map.filterWithKey keepColumn (catalogObjects catalog)
        }
    where
        keepColumn (ColumnKey owner _) object =
            owner /= table || objectDefinition object == Just sid
        keepColumn _ _ = True

moveTableColumns :: StatementId -> ObjectKey -> ObjectKey -> Catalog -> Catalog -> Catalog
moveTableColumns sid oldKey newKey sourceCatalog targetCatalog =
    let sourceColumns =
            [ column
            | (ColumnKey owner column, object) <- Map.toList (catalogObjects sourceCatalog)
            , owner == oldKey
            , not (objectDropped object)
            ]
        cleanTarget = resetTableColumns sid newKey targetCatalog
        movedObjects =
            foldl'
                ( \objects column -> Map.insert (ColumnKey newKey column) (ObjectState (Just sid) Nothing Set.empty False) objects
                )
                (catalogObjects cleanTarget)
                sourceColumns
     in cleanTarget {catalogObjects = movedObjects}

bindRelations :: StatementId -> ObjectKey -> [ObjectAction] -> Catalog -> Catalog
bindRelations _ key actions catalog =
    let tables =
            [ owner
            | action <- actions
            , actionKind action == TableObject
            , actionType action == UseDep
            , owner <- matchingKeysInCatalog catalog TableObject UseDep (actionName action)
            , isLiveKey catalog owner
            ]
        withoutOldBinding =
            Map.mapMaybe
                (\bound -> let kept = Set.delete key bound in if Set.null kept then Nothing else Just kept)
                (catalogRelations catalog)
        relations' =
            foldl'
                (\relations table -> Map.insertWith Set.union table (Set.singleton key) relations)
                withoutOldBinding
                tables
     in catalog {catalogRelations = relations'}

boundIndexDependencies :: Catalog -> ObjectName -> Set StatementId
boundIndexDependencies catalog table =
    Set.unions
        [ boundDependencies catalog ((== IndexObject) . objectKind) owner
        | owner <- matchingKeysInCatalog catalog TableObject UseDep table
        ]

boundObjectDependencies :: Catalog -> ObjectName -> Set StatementId
boundObjectDependencies catalog table =
    Set.unions
        [ boundDependencies catalog (const True) owner
        | owner <- matchingKeysInCatalog catalog TableObject ModDep table
        ]

boundDependencies :: Catalog -> (ObjectKey -> Bool) -> ObjectKey -> Set StatementId
boundDependencies catalog include owner =
    Set.unions
        [ maybe Set.empty stateDependenciesInObject (Map.lookup key (catalogObjects catalog))
        | key <- Set.toList (Map.findWithDefault Set.empty owner (catalogRelations catalog))
        , include key
        ]

viewSources :: Catalog -> [ObjectAction] -> ObjectName -> Set (Either ObjectName ObjectKey)
viewSources catalog actions view =
    if any
        (\act -> actionKind act == ViewObject && actionType act == DefDep && actionName act == view)
        actions
        then
            Set.fromList
                [ source
                | act <- actions
                , actionKind act == TableObject
                , actionType act == UseDep
                , source <- case matchingKeysInCatalog catalog TableObject UseDep (actionName act) of
                    [] -> [Left (actionName act)]
                    keys -> map Right keys
                ]
        else Set.empty

updateViewSources :: ObjectKey -> ObjectKey -> Catalog -> Catalog
updateViewSources oldKey newKey catalog =
    catalog
        { catalogObjects =
            Map.map
                ( \object -> object {objectSources = Set.unions (map replaceSource (Set.toList (objectSources object)))}
                )
                (catalogObjects catalog)
        }
    where
        replaceSource (Right key) =
            Set.singleton (Right (if key == oldKey then newKey else key))
        replaceSource source@(Left name) =
            case (oldKey, newKey) of
                (ObjectKey _ oldName, ObjectKey _ newName)
                    | namesMayReferToEachOther oldName name ->
                        Set.fromList [source, Left newName]
                _ -> Set.singleton source

moveRelations ::
    ObjectKey ->
    ObjectKey ->
    Map ObjectKey (Set ObjectKey) ->
    Map ObjectKey (Set ObjectKey) ->
    Map ObjectKey (Set ObjectKey)
moveRelations oldKey newKey sourceRelations targetRelations =
    case Map.lookup oldKey sourceRelations of
        Nothing -> targetRelations
        Just bound -> Map.insertWith Set.union newKey bound targetRelations

applyTransactionControl ::
    StatementId -> [ObjectAction] -> ScanState -> (Set StatementId, ScanState)
applyTransactionControl _sid actions state =
    case mapMaybe (txCommandFromAction . actionName) actions of
        command : _ -> apply command
        [] -> (Set.empty, state)
    where
        snapshot = scanCatalog state

        apply TxBegin =
            case scanTransactions state of
                [] ->
                    ( Set.empty
                    , state {scanTransactions = [TxFrame snapshot [] False]}
                    )
                _ -> (Set.empty, state)
        apply TxCommit =
            (Set.empty, state {scanTransactions = drop 1 (scanTransactions state)})
        apply TxRollback =
            case scanTransactions state of
                frame : rest ->
                    ( Set.empty
                    , state
                        { scanCatalog = txStart frame
                        , scanTransactions = rest
                        }
                    )
                [] -> (Set.empty, state)
        apply (TxSavepoint name) =
            case scanTransactions state of
                frame : rest ->
                    ( Set.empty
                    , state
                        { scanTransactions =
                            frame {txSavepoints = txSavepoints frame ++ [(name, snapshot)]} : rest
                        }
                    )
                [] ->
                    ( Set.empty
                    , state
                        { scanTransactions =
                            [TxFrame snapshot [(name, snapshot)] True]
                        }
                    )
        apply (TxRollbackTo name) =
            case scanTransactions state of
                frame : rest ->
                    case break ((== name) . fst) (reverse (txSavepoints frame)) of
                        (_, saved : older) ->
                            let retained = reverse older ++ [saved]
                             in ( Set.empty
                                , state
                                    { scanCatalog = snd saved
                                    , scanTransactions = frame {txSavepoints = retained} : rest
                                    }
                                )
                        _ -> (Set.empty, state)
                [] -> (Set.empty, state)
        apply (TxRelease name) =
            case scanTransactions state of
                frame : rest ->
                    case break ((== name) . fst) (reverse (txSavepoints frame)) of
                        (_, _ : older) ->
                            let retained = reverse older
                                transactions =
                                    if txImplicitSavepoint frame && null retained
                                        then rest
                                        else frame {txSavepoints = retained} : rest
                             in (Set.empty, state {scanTransactions = transactions})
                        _ -> (Set.empty, state)
                [] -> (Set.empty, state)

txCommandFromAction :: ObjectName -> Maybe TxCommand
txCommandFromAction "__tx_begin__" = Just TxBegin
txCommandFromAction "__tx_commit__" = Just TxCommit
txCommandFromAction "__tx_rollback__" = Just TxRollback
txCommandFromAction name =
    case T.stripPrefix "__tx_savepoint__\x1e" name of
        Just label -> Just (TxSavepoint label)
        Nothing ->
            case T.stripPrefix "__tx_rollback_to__\x1e" name of
                Just label -> Just (TxRollbackTo label)
                Nothing ->
                    TxRelease <$> T.stripPrefix "__tx_release__\x1e" name

-- | End-to-end analyzer; the existing entry point keeps its generic SQL rules.
analyzeStatements :: [StatementUnit] -> ByteString -> IO DependencyGraph
analyzeStatements = analyzeStatementsWithDialect defaultSQLDialect

analyzeStatementsWithDialect ::
    SQLDialect ->
    [StatementUnit] ->
    ByteString ->
    IO DependencyGraph
analyzeStatementsWithDialect dialect units src = do
    stmtActions <- mapM extractUnit units
    pure (buildGraph stmtActions)
    where
        extractUnit unit = do
            let sid = stmtId (unitStatement unit)
            actions <- extractActionsWithDialect dialect unit src
            pure (sid, actions)
