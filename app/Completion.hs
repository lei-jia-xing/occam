module Completion (
    ShellType (..),
    parseShellType,
    generateCompletion,
) where

import Data.Char (toLower)
import Data.List (intercalate)

data ShellType = ShellBash | ShellZsh | ShellFish
    deriving (Eq, Show)

parseShellType :: String -> Either String ShellType
parseShellType s = case map toLower s of
    "bash" -> Right ShellBash
    "zsh" -> Right ShellZsh
    "fish" -> Right ShellFish
    _ -> Left "Expected 'bash', 'zsh', or 'fish'"

-- | How an option consumes its value, and which completions it offers.
data ValueSpec
    = FlagValue
    | CommandValue String
    | FileValue String
    | NumberValue String
    | ChoiceValue String [String]
    deriving (Eq, Show)

-- | One CLI option rendered into every supported shell.
data OptionSpec = OptionSpec
    { optionShort :: !(Maybe Char)
    , optionLong :: ![String]
    , optionDescription :: !String
    , optionValue :: !ValueSpec
    }
    deriving (Eq, Show)

dialects :: [String]
dialects =
    [ "generic"
    , "postgresql"
    , "mysql"
    , "sqlite"
    , "duckdb"
    , "clickhouse"
    , "oracle"
    , "ansi"
    , "bigquery"
    , "databricks"
    , "hive"
    , "mssql"
    , "redshift"
    , "snowflake"
    , "spark"
    , "teradata"
    ]

shells :: [String]
shells = ["bash", "zsh", "fish"]

-- | Supported completion options, descriptions, and value specifications.
optionSpecs :: [OptionSpec]
optionSpecs =
    [ OptionSpec (Just 'c') ["oracle"] "Interestingness predicate command" (CommandValue "COMMAND")
    , OptionSpec (Just 'o') ["out"] "Output SQL file path, or - for stdout" (FileValue "OUTPUT FILE")
    , OptionSpec (Just 'i') ["in-place"] "Modify input SQL file in place" FlagValue
    , OptionSpec Nothing ["stats"] "Write JSON reduction statistics to FILE" (FileValue "FILE")
    , OptionSpec Nothing ["reset"] "Lightweight state reset command" (CommandValue "RESET COMMAND")
    , OptionSpec Nothing ["timeout"] "Per-execution timeout in seconds" (NumberValue "SECONDS")
    , OptionSpec
        Nothing
        ["log-level"]
        "Log severity level"
        (ChoiceValue "LEVEL" ["debug", "info", "warn", "error"])
    , OptionSpec Nothing ["log-format"] "Log output format" (ChoiceValue "FORMAT" ["text", "json"])
    , OptionSpec Nothing ["verbose"] "Enable verbose debug output" FlagValue
    , OptionSpec (Just 'q') ["quiet"] "Suppress info logs and banners" FlagValue
    , OptionSpec
        Nothing
        ["color"]
        "Colorized terminal output"
        (ChoiceValue "MODE" ["auto", "always", "never"])
    , OptionSpec Nothing ["budget", "time-limit"] "Shared wall-clock cap" (NumberValue "SECONDS")
    , OptionSpec
        Nothing
        ["max-oracle-executions", "max-evals"]
        "Maximum oracle attempts"
        (NumberValue "N")
    , OptionSpec (Just 'j') ["jobs"] "Maximum concurrent oracle executions" (NumberValue "N")
    , OptionSpec
        Nothing
        ["strategy"]
        "Reduction strategy"
        (ChoiceValue "STRATEGY" ["full", "ddmin-only", "audit-only"])
    , OptionSpec
        Nothing
        ["column-mode"]
        "Column co-reduction mode"
        (ChoiceValue "MODE" ["off", "single", "atomic"])
    , OptionSpec (Just 'd') ["dialect"] "SQL dependency dialect" (ChoiceValue "DIALECT" dialects)
    , OptionSpec Nothing ["slicing"] "Enable dependency slicing" FlagValue
    , OptionSpec Nothing ["no-slicing"] "Disable dependency slicing" FlagValue
    , OptionSpec Nothing ["closure"] "Enable dependency closure" FlagValue
    , OptionSpec Nothing ["no-closure"] "Disable dependency closure" FlagValue
    , OptionSpec Nothing ["intra-statement"] "Enable intra-statement reductions" FlagValue
    , OptionSpec Nothing ["no-intra-statement"] "Disable intra-statement reductions" FlagValue
    , OptionSpec Nothing ["unrestricted"] "Enable unrestricted fallback search" FlagValue
    , OptionSpec Nothing ["no-unrestricted"] "Disable unrestricted fallback search" FlagValue
    , OptionSpec Nothing ["cache"] "Enable candidate caching" FlagValue
    , OptionSpec Nothing ["no-cache"] "Disable candidate caching" FlagValue
    , OptionSpec Nothing ["list-dialects"] "List supported SQL dialects" FlagValue
    , OptionSpec Nothing ["list-column-modes"] "List supported column modes" FlagValue
    , OptionSpec Nothing ["list-strategies"] "List supported reduction strategies" FlagValue
    , OptionSpec Nothing ["print-defaults"] "Print default settings in JSON format" FlagValue
    , OptionSpec
        Nothing
        ["completion"]
        "Generate shell autocompletion script"
        (ChoiceValue "SHELL" shells)
    , OptionSpec (Just 'v') ["version"] "Show version information" FlagValue
    , OptionSpec (Just 'h') ["help"] "Show this help text" FlagValue
    ]

generateCompletion :: ShellType -> String
generateCompletion ShellBash = bashCompletionScript
generateCompletion ShellZsh = zshCompletionScript
generateCompletion ShellFish = fishCompletionScript

optionAliases :: OptionSpec -> [String]
optionAliases spec =
    maybe [] (pure . shortAlias) (optionShort spec)
        ++ map ("--" ++) (optionLong spec)

shortAlias :: Char -> String
shortAlias short = ['-', short]

-- | Render a zsh @_arguments@ specification.
zshOption :: OptionSpec -> String
zshOption spec = case optionAliases spec of
    [alias] -> "'" ++ alias ++ zshDescription spec ++ "'"
    aliases ->
        "'("
            ++ unwords aliases
            ++ ")'{"
            ++ intercalate "," aliases
            ++ "}'"
            ++ zshDescription spec
            ++ "'"

zshDescription :: OptionSpec -> String
zshDescription spec =
    "[" ++ optionDescription spec ++ "]" ++ zshValue (optionValue spec)

zshValue :: ValueSpec -> String
zshValue FlagValue = ""
zshValue (CommandValue metavar) = ":" ++ metavar ++ ":_command_args"
zshValue (FileValue metavar) = ":" ++ metavar ++ ":_files"
zshValue (NumberValue metavar) = ":" ++ metavar ++ ":"
zshValue (ChoiceValue metavar choices) = ":" ++ metavar ++ ":(" ++ unwords choices ++ ")"

-- | Render an option as a fish @complete@ invocation.
fishOption :: OptionSpec -> String
fishOption spec =
    "complete -c occam"
        ++ maybe "" (\short -> " -s " ++ [short]) (optionShort spec)
        ++ concatMap (" -l " ++) (optionLong spec)
        ++ " -d \""
        ++ optionDescription spec
        ++ "\""
        ++ fishValue (optionValue spec)

fishValue :: ValueSpec -> String
fishValue FlagValue = ""
fishValue (CommandValue _) = " -r"
fishValue (FileValue _) = " -r -F"
fishValue (NumberValue _) = " -r"
fishValue (ChoiceValue _ choices) = " -r -a \"" ++ unwords choices ++ "\""

zshCompletionScript :: String
zshCompletionScript =
    unlines
        ( [ "#compdef occam"
          , ""
          , "_occam() {"
          , "  local -a args"
          , "  args=("
          ]
            ++ map (\spec -> "    " ++ zshOption spec) optionSpecs
            ++ [ "    '1:INPUT.sql:_files'"
               , "  )"
               , "  _arguments -s -S $args"
               , "}"
               , ""
               , "_occam \"$@\""
               ]
        )

fishCompletionScript :: String
fishCompletionScript =
    unlines
        ( "# Fish completion for occam"
            : map fishOption optionSpecs
        )

bashCompletionScript :: String
bashCompletionScript =
    unlines
        ( [ "# This script enables bash autocompletion for occam."
          , "# To enable bash completion, add this to your .bashrc:"
          , "# eval \"$(occam --completion=bash)\""
          , ""
          , "_occam()"
          , "{"
          , "    local cur prev opts"
          , "    COMPREPLY=()"
          , "    cur=\"${COMP_WORDS[COMP_CWORD]}\""
          , "    prev=\"${COMP_WORDS[COMP_CWORD-1]}\""
          , ""
          , "    opts=\"" ++ unwords (concatMap optionAliases optionSpecs) ++ "\""
          , ""
          , "    case \"${prev}\" in"
          ]
            ++ concatMap bashValueCase optionSpecs
            ++ [ "        *)"
               , "            ;;"
               , "    esac"
               , ""
               , "    if [[ \"${cur}\" == -* ]] ; then"
               , "        COMPREPLY=( $(compgen -W \"${opts}\" -- \"${cur}\") )"
               , "        return 0"
               , "    else"
               , "        COMPREPLY=( $(compgen -f -- \"${cur}\") )"
               , "        return 0"
               , "    fi"
               , "}"
               , ""
               , "complete -F _occam -o default occam"
               ]
        )
    where
        bashValueCase spec = case optionValue spec of
            CommandValue _ -> bashArm spec []
            FileValue _ -> bashArm spec ["            COMPREPLY=( $(compgen -f -- \"${cur}\") )"]
            ChoiceValue _ choices ->
                bashArm spec ["            COMPREPLY=( $(compgen -W \"" ++ unwords choices ++ "\" -- \"${cur}\") )"]
            FlagValue -> []
            NumberValue _ -> []
        bashArm spec body =
            ["        " ++ intercalate "|" (optionAliases spec) ++ ")"]
                ++ body
                ++ [ "            return 0"
                   , "            ;;"
                   ]
