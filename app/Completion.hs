module Completion (
    ShellType (..),
    parseShellType,
    supportedShells,
    generateCompletion,
    generateCompletionFor,
) where

import Data.Char (toLower)
import Options.Applicative.BashCompletion (
    bashCompletionScript,
    fishCompletionScript,
    zshCompletionScript,
 )

-- | Target shells for generated autocompletion scripts.
data ShellType = ShellBash | ShellZsh | ShellFish
    deriving (Eq, Show, Bounded, Enum)

-- | Names and associated constructors of supported shells.
supportedShells :: [(String, ShellType)]
supportedShells =
    [ ("bash", ShellBash)
    , ("zsh", ShellZsh)
    , ("fish", ShellFish)
    ]

-- | Parse a shell name string into a 'ShellType'.
parseShellType :: String -> Either String ShellType
parseShellType s = case map toLower s of
    "bash" -> Right ShellBash
    "zsh" -> Right ShellZsh
    "fish" -> Right ShellFish
    _ -> Left "Expected 'bash', 'zsh', or 'fish'"

-- | Generate native shell completion script for a program using optparse-applicative.
generateCompletionFor :: String -> ShellType -> String
generateCompletionFor prog ShellBash = bashCompletionScript prog prog
generateCompletionFor prog ShellZsh = zshCompletionScript prog prog
generateCompletionFor prog ShellFish = fishCompletionScript prog prog

-- | Generate native shell completion script for @occam@.
generateCompletion :: ShellType -> String
generateCompletion = generateCompletionFor "occam"
