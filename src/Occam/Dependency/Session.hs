-- | Session effects: SET/RESET variables, search paths, and roles.
module Occam.Dependency.Session (
    applySessionEffects,
    applySessionEffect,
    setSearchPath,
    setSessionUser,
    setCurrentUser,
    setVariable,
) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Occam.Core (
    Ident,
    Name (..),
    SearchPathValue (..),
    SessionEffect (..),
    SessionScope (..),
    SessionValue (..),
    StatementId,
    nameText,
 )
import Occam.Dependency.Query (namespaceDeps)
import Occam.Dependency.State (
    BarrierKey (..),
    Eval (..),
    SessionState (..),
    addDeps,
    effectiveNamespacePrefix,
    effectiveSearchPath,
    effectiveSessionUser,
 )

applySessionEffects :: StatementId -> [SessionEffect] -> Eval -> (Eval, Bool)
applySessionEffects _ effects eval =
    let eval' =
            foldl'
                ( \current effect ->
                    let session' = applySessionEffect effect (evalSession current)
                        schemaDeps =
                            case effect of
                                SetNamespace _ _ -> namespaceDeps (evalCatalog current) (effectiveNamespacePrefix session')
                                SetSearchPath _ _ -> searchPathDeps current session'
                                SetSessionUser _ _ -> searchPathDeps current session'
                                ResetSessionUser _ -> searchPathDeps current session'
                                SetCurrentUser _ _ -> searchPathDeps current session'
                                ResetCurrentUser _ -> searchPathDeps current session'
                                _ -> Set.empty
                     in (addDeps schemaDeps current)
                            { evalSession = session'
                            , evalTouches = Set.insert SessionBarrierKey (evalTouches current)
                            , evalSessionTouched = True
                            }
                )
                eval
                effects
     in (eval', evalSessionTouched eval')
    where
        searchPathDeps current session =
            Set.unions [namespaceDeps (evalCatalog current) parts | Name parts <- effectiveSearchPath session]

applySessionEffect :: SessionEffect -> SessionState -> SessionState
applySessionEffect effect session =
    case effect of
        SetVariable scope name value ->
            setVariable scope (nameText name) value session
        SetSearchPath scope path ->
            setSearchPath scope path session
        SetNamespace scope prefix ->
            case scope of
                ScopeSession -> session {sessNamespacePrefix = prefix}
                -- Global namespace changes are intentionally unsupported.
                ScopeGlobal -> session
                _ -> session {sessTxNamespacePrefix = Just prefix}
        SetSessionUser scope user ->
            setSessionUser scope (Just user) session
        ResetSessionUser scope ->
            setSessionUser scope (sessAuthenticatedUser session) session
        SetCurrentUser scope user ->
            setCurrentUser scope (Just user) session
        ResetCurrentUser scope ->
            setCurrentUser scope (effectiveSessionUser session) session
        SetAutocommit scope value ->
            case scope of
                ScopeSession -> session {sessAutocommit = value}
                _ -> session
        SetForeignKeyChecks scope value ->
            case scope of
                ScopeSession -> session {sessForeignKeyChecks = value}
                _ -> session
        ResetForeignKeyChecks scope ->
            case scope of
                ScopeSession ->
                    session
                        { sessForeignKeyChecks =
                            sessInitialForeignKeyChecks session
                        }
                _ -> session
        ResetVariable scope name ->
            let key = nameText name
             in case scope of
                    ScopeSession ->
                        session {sessVariables = Map.delete key (sessVariables session)}
                    -- Global variable resets are intentionally unsupported.
                    ScopeGlobal -> session
                    _ ->
                        session {sessTxVariables = Map.delete key (sessTxVariables session)}
        ResetAll scope ->
            case scope of
                ScopeSession ->
                    session
                        { sessSearchPath = []
                        , sessVariables = Map.empty
                        , sessForeignKeyChecks =
                            sessInitialForeignKeyChecks session
                        }
                -- RESET ALL with global scope is intentionally unsupported.
                ScopeGlobal -> session
                _ ->
                    session
                        { sessTxSearchPath = Nothing
                        , sessTxNamespacePrefix = Nothing
                        , sessTxCurrentUser = Nothing
                        , sessTxVariables = Map.empty
                        }

setSearchPath :: SessionScope -> SearchPathValue -> SessionState -> SessionState
setSearchPath scope path session =
    case scope of
        ScopeSession -> session {sessSearchPath = entries}
        -- Global search paths are intentionally unsupported.
        ScopeGlobal -> session
        _ -> session {sessTxSearchPath = Just entries}
    where
        entries =
            case path of
                SearchPathDefault -> []
                SearchPathExplicit values -> values

-- | @SET SESSION AUTHORIZATION@ changes both session_user and current_user.
setSessionUser :: SessionScope -> Maybe Ident -> SessionState -> SessionState
setSessionUser scope user session =
    case scope of
        ScopeSession ->
            session
                { sessSessionUser = user
                , sessCurrentUser = user
                }
        -- Global role changes are intentionally unsupported.
        ScopeGlobal -> session
        _ ->
            session
                { sessTxSessionUser = Just user
                , sessTxCurrentUser = Just user
                }

setCurrentUser :: SessionScope -> Maybe Ident -> SessionState -> SessionState
setCurrentUser scope user session =
    case scope of
        ScopeSession -> session {sessCurrentUser = user}
        -- Global current-user changes are intentionally unsupported.
        ScopeGlobal -> session
        _ -> session {sessTxCurrentUser = Just user}

setVariable :: SessionScope -> Text -> SessionValue -> SessionState -> SessionState
setVariable scope key value session =
    let update current =
            case value of
                SessionDefault -> Map.delete key current
                _ -> Map.insert key value current
     in case scope of
            ScopeSession -> session {sessVariables = update (sessVariables session)}
            -- Global variables are intentionally unsupported.
            ScopeGlobal -> session
            _ -> session {sessTxVariables = update (sessTxVariables session)}
