-- Typed experiment plans and default oracle settings for Stage 2 evaluation.
let T = ./Types.dhall

let oracleDefaults
    : T.OracleDefaults
    = { timeout_secs = 0.0
      , stack_frames = 5
      , reject_on_slippage = False
      , allow_stderr_only = False
      , allow_psql_meta = False
      , verbose = False
      }

let allDbms
    : List T.DbmsId
    = [ T.DbmsId.postgres
      , T.DbmsId.mysql
      , T.DbmsId.mariadb
      , T.DbmsId.clickhouse
      , T.DbmsId.sqlite
      , T.DbmsId.duckdb
      ]

let smokePlan
    : T.ExperimentPlan
    = { name = "smoke"
      , repeats = 1
      , worker_jobs = 1
      , occam_jobs = 1
      , timeout_secs = 10.0
      , budget_secs = None Double
      , max_oracle_executions = None Natural
      , no_cache = False
      , container_wait_secs = 30
      , profiles = [ "Ours-S" ]
      , dbms = allDbms
      , oracle = oracleDefaults
      }

let stage2ThesisPlan
    : T.ExperimentPlan
    = { name = "stage2-thesis"
      , repeats = 3
      , worker_jobs = 4
      , occam_jobs = 1
      , timeout_secs = 10.0
      , budget_secs = Some 600.0
      , max_oracle_executions = None Natural
      , no_cache = False
      , container_wait_secs = 30
      , profiles =
          [ "audit-only"
          , "ddmin-only"
          , "Ours-S"
          , "Ours-SI"
          , "Ours-Full"
          , "Ours-NoDep"
          ]
      , dbms = allDbms
      , oracle = oracleDefaults // { reject_on_slippage = True }
      }

in  { oracleDefaults, smokePlan, stage2ThesisPlan }
