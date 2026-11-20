# This script enables bash autocompletion for occam.
# To enable bash completion, add this to your .bashrc:
# eval "$(occam --completion=bash)"

_occam()
{
    local cur prev opts dialects column_modes strategies log_levels log_formats shells colors
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    opts="-c --oracle -o --out -i --in-place --stats --reset --timeout \
          --log-level --log-format --verbose -q --quiet --color --budget \
          --time-limit --max-oracle-executions --max-evals -j --jobs \
          --strategy --column-mode -d --dialect --slicing --no-slicing \
          --closure --no-closure --intra-statement --no-intra-statement \
          --unrestricted --no-unrestricted --cache --no-cache \
          --list-dialects --list-column-modes --list-strategies \
          --print-defaults --completion -v --version -h --help"

    dialects="generic postgresql mysql sqlite duckdb clickhouse oracle ansi bigquery databricks hive mssql redshift snowflake spark teradata"
    column_modes="off single atomic"
    strategies="full ddmin-only audit-only"
    log_levels="debug info warn error"
    log_formats="text json"
    colors="auto always never"
    shells="bash zsh fish"

    case "${prev}" in
        -c|--oracle|--reset)
            return 0
            ;;
        -o|--out|--stats)
            COMPREPLY=( $(compgen -f -- "${cur}") )
            return 0
            ;;
        -d|--dialect)
            COMPREPLY=( $(compgen -W "${dialects}" -- "${cur}") )
            return 0
            ;;
        --column-mode)
            COMPREPLY=( $(compgen -W "${column_modes}" -- "${cur}") )
            return 0
            ;;
        --strategy)
            COMPREPLY=( $(compgen -W "${strategies}" -- "${cur}") )
            return 0
            ;;
        --log-level)
            COMPREPLY=( $(compgen -W "${log_levels}" -- "${cur}") )
            return 0
            ;;
        --log-format)
            COMPREPLY=( $(compgen -W "${log_formats}" -- "${cur}") )
            return 0
            ;;
        --color)
            COMPREPLY=( $(compgen -W "${colors}" -- "${cur}") )
            return 0
            ;;
        --completion)
            COMPREPLY=( $(compgen -W "${shells}" -- "${cur}") )
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
