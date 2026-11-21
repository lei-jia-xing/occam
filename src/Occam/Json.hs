module Occam.Json (
    camelToSnake,
    normalizeJsonFieldNames,
    snakeCaseOptions,
) where

import Data.Aeson (Options (..), Value (..), defaultOptions)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isUpper, toLower)

-- | Convert a camelCase field name to snake_case, preserving the whole name.
camelToSnake :: String -> String
camelToSnake = concatMap convert
    where
        convert character
            | isUpper character = ['_', toLower character]
            | otherwise = [character]

-- | Generic JSON options using snake_case field names.
snakeCaseOptions :: Options
snakeCaseOptions = defaultOptions {fieldLabelModifier = camelToSnake}

{- | Rewrite top-level object keys to snake_case. Nested objects keep their
keys so dynamic maps (status names, family tags) are never rewritten.
-}
normalizeJsonFieldNames :: Value -> Value
normalizeJsonFieldNames (Object fields) =
    Object
        ( KeyMap.fromList
            [ (Key.fromString (camelToSnake (Key.toString name)), value)
            | (name, value) <- KeyMap.toList fields
            ]
        )
normalizeJsonFieldNames value = value
