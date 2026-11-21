# Fish completion for occam
complete -c occam -s c -l oracle -d "Interestingness predicate command" -r
complete -c occam -s o -l out -d "Output SQL file path, or - for stdout" -r -F
complete -c occam -s i -l in-place -d "Modify input SQL file in place"
complete -c occam -l stats -d "Write JSON reduction statistics to FILE" -r -F
complete -c occam -l reset -d "Lightweight state reset command" -r
complete -c occam -l timeout -d "Per-execution timeout in seconds" -r
complete -c occam -l log-level -d "Log severity level" -r -a "debug info warn error"
complete -c occam -l log-format -d "Log output format" -r -a "text json"
complete -c occam -l verbose -d "Enable verbose debug output"
complete -c occam -s q -l quiet -d "Suppress info logs and banners"
complete -c occam -l color -d "Colorized terminal output" -r -a "auto always never"
complete -c occam -l budget -l time-limit -d "Shared wall-clock cap" -r
complete -c occam -l max-oracle-executions -l max-evals -d "Maximum oracle attempts" -r
complete -c occam -s j -l jobs -d "Maximum concurrent oracle executions" -r
complete -c occam -l strategy -d "Reduction strategy" -r -a "full ddmin-only audit-only"
complete -c occam -l column-mode -d "Column co-reduction mode" -r -a "off single atomic"
complete -c occam -s d -l dialect -d "SQL dependency dialect" -r -a "generic postgresql mysql sqlite duckdb clickhouse oracle ansi bigquery databricks hive mssql redshift snowflake spark teradata"
complete -c occam -l slicing -d "Enable dependency slicing"
complete -c occam -l no-slicing -d "Disable dependency slicing"
complete -c occam -l closure -d "Enable dependency closure"
complete -c occam -l no-closure -d "Disable dependency closure"
complete -c occam -l intra-statement -d "Enable intra-statement reductions"
complete -c occam -l no-intra-statement -d "Disable intra-statement reductions"
complete -c occam -l unrestricted -d "Enable unrestricted fallback search"
complete -c occam -l no-unrestricted -d "Disable unrestricted fallback search"
complete -c occam -l cache -d "Enable candidate caching"
complete -c occam -l no-cache -d "Disable candidate caching"
complete -c occam -l list-dialects -d "List supported SQL dialects"
complete -c occam -l list-column-modes -d "List supported column modes"
complete -c occam -l list-strategies -d "List supported reduction strategies"
complete -c occam -l print-defaults -d "Print default settings in JSON format"
complete -c occam -l completion -d "Generate shell autocompletion script" -r -a "bash zsh fish"
complete -c occam -s v -l version -d "Show version information"
complete -c occam -s h -l help -d "Show this help text"
