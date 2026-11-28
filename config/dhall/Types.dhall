-- Shared Dhall schema types for Occam CLI configs and multi-DBMS evaluation harness.
let Dialect =
      < postgresql | mysql | mariadb | sqlite | duckdb | clickhouse >

let DbmsId =
      < postgres | mysql | mariadb | sqlite | duckdb | clickhouse >

let Strategy = < full | ddmin-only | audit-only >

let StatementOrder =
      < ranked-fallback | ranked | byte-weighted | complement-first | subset-first >

let ColumnMode = < off | atomic >

let LogLevel = < debug | info | warn | error >

let LogFormat = < text | json >

let ColorMode = < auto | always | never >

let OccamDefaults =
      { timeout : Double
      , jobs : Natural
      , strategy : Strategy
      , statement_order : StatementOrder
      , column_mode : ColumnMode
      , slicing : Bool
      , closure : Bool
      , intra_statement : Bool
      , literal_batches : Bool
      , ast_batching : Bool
      , adaptive_values : Bool
      , unrestricted : Bool
      , cache : Bool
      , log_level : LogLevel
      , log_level_json : LogLevel
      , log_format : LogFormat
      , color : ColorMode
      }

let OccamConfig =
      { dialect : Optional Dialect
      , strategy : Optional Strategy
      , slicing : Optional Bool
      , closure : Optional Bool
      , intra_statement : Optional Bool
      , statement_order : Optional StatementOrder
      , literal_batches : Optional Bool
      , ast_batching : Optional Bool
      , adaptive_values : Optional Bool
      , column_mode : Optional ColumnMode
      , unrestricted : Optional Bool
      , cache : Optional Bool
      , timeout : Optional Double
      , jobs : Optional Natural
      , budget : Optional Double
      , max_oracle_executions : Optional Natural
      , result_oracle : Optional Bool
      , oracle : Optional Text
      , reset : Optional Text
      , out : Optional Text
      , in_place : Optional Bool
      , stats : Optional Text
      , log_level : Optional LogLevel
      , log_format : Optional LogFormat
      , verbose : Optional Bool
      , quiet : Optional Bool
      , color : Optional ColorMode
      }

let DbmsSpec =
      { id : DbmsId
      , occam_dialect : Dialect
      , server_based : Bool
      , container_image : Text
      , default_port : Optional Natural
      , container_log : Optional Text
      , default_host : Optional Text
      , default_user : Optional Text
      , default_db : Text
      , sqlancer_subcommand : Optional Text
      , sqlancer_port : Optional Natural
      , squirrel_db : Optional Text
      }

let AblationProfile =
      { name : Text
      , key : Text
      , config : OccamConfig
      , cli_flags : List Text
      }

let OracleDefaults =
      { timeout_secs : Double
      , stack_frames : Natural
      , reject_on_slippage : Bool
      , allow_stderr_only : Bool
      , allow_psql_meta : Bool
      , verbose : Bool
      }

let SquirrelProfile =
      { dbms : Text
      , mutator_dbms : Text
      , default_input_subdir : Text
      , target_bin_rel : Text
      , db : Text
      , required : List Text
      , should_exist : List Text
      , init_lib_rel : Text
      , data_lib_rel : Optional Text
      , pragma_rel : Optional Text
      , default_host : Optional Text
      , default_port : Optional Natural
      , default_user : Optional Text
      , default_db : Optional Text
      , default_socket : Optional Text
      }

let ExperimentPlan =
      { name : Text
      , repeats : Natural
      , worker_jobs : Natural
      , occam_jobs : Natural
      , timeout_secs : Double
      , budget_secs : Optional Double
      , max_oracle_executions : Optional Natural
      , no_cache : Bool
      , container_wait_secs : Natural
      , profiles : List Text
      , dbms : List DbmsId
      , oracle : OracleDefaults
      }

in  { Dialect
    , DbmsId
    , Strategy
    , StatementOrder
    , ColumnMode
    , LogLevel
    , LogFormat
    , ColorMode
    , OccamDefaults
    , OccamConfig
    , DbmsSpec
    , AblationProfile
    , OracleDefaults
    , SquirrelProfile
    , ExperimentPlan
    }
