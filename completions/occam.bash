# This script enables bash autocompletion for occam.
# To enable bash completion, add this to your .bashrc:
# eval "$(occam --completion=bash)"

_occam()
{
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    opts="-c --oracle -o --out -i --in-place --stats --reset --timeout --log-level --log-format --verbose -q --quiet --color --budget --time-limit --max-oracle-executions --max-evals -j --jobs --strategy --column-mode -d --dialect --slicing --no-slicing --closure --no-closure --intra-statement --no-intra-statement --unrestricted --no-unrestricted --cache --no-cache --list-dialects --list-column-modes --list-strategies --print-defaults --completion -v --version -h --help"

    case "${prev}" in
        -c|--oracle)
            return 0
            ;;
        -o|--out)
            COMPREPLY=( $(compgen -f -- "${cur}") )
            return 0
            ;;
        --stats)
            COMPREPLY=( $(compgen -f -- "${cur}") )
            return 0
            ;;
        --reset)
            return 0
            ;;
        --log-level)
            COMPREPLY=( $(compgen -W "debug info warn error" -- "${cur}") )
            return 0
            ;;
        --log-format)
            COMPREPLY=( $(compgen -W "text json" -- "${cur}") )
            return 0
            ;;
        --color)
            COMPREPLY=( $(compgen -W "auto always never" -- "${cur}") )
            return 0
            ;;
        --strategy)
            COMPREPLY=( $(compgen -W "full ddmin-only audit-only" -- "${cur}") )
            return 0
            ;;
        --column-mode)
            COMPREPLY=( $(compgen -W "off single atomic" -- "${cur}") )
            return 0
            ;;
        -d|--dialect)
            COMPREPLY=( $(compgen -W "generic postgresql mysql sqlite duckdb clickhouse oracle ansi bigquery databricks hive mssql redshift snowflake spark teradata" -- "${cur}") )
            return 0
            ;;
        --completion)
            COMPREPLY=( $(compgen -W "bash zsh fish" -- "${cur}") )
            return 0
            ;;
        *)
            ;;
    esac

    if [[ "${cur}" == -* ]] ; then
        COMPREPLY=( $(compgen -W "${opts}" -- "${cur}") )
        return 0
    else
        COMPREPLY=( $(compgen -f -- "${cur}") )
        return 0
    fi
}

complete -F _occam -o default occam
