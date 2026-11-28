-- Canonical Occam CLI defaults and typed config-to-CLI rendering helpers.
let T = ./Types.dhall

let defaults
    : T.OccamDefaults
    = { timeout = 10.0
      , jobs = 1
      , strategy = T.Strategy.full
      , statement_order = T.StatementOrder.ranked-fallback
      , column_mode = T.ColumnMode.off
      , slicing = True
      , closure = True
      , intra_statement = False
      , literal_batches = False
      , ast_batching = False
      , adaptive_values = False
      , unrestricted = True
      , cache = True
      , log_level = T.LogLevel.warn
      , log_level_json = T.LogLevel.info
      , log_format = T.LogFormat.text
      , color = T.ColorMode.auto
      }

let emptyConfig
    : T.OccamConfig
    = { dialect = None T.Dialect
      , strategy = None T.Strategy
      , slicing = None Bool
      , closure = None Bool
      , intra_statement = None Bool
      , statement_order = None T.StatementOrder
      , literal_batches = None Bool
      , ast_batching = None Bool
      , adaptive_values = None Bool
      , column_mode = None T.ColumnMode
      , unrestricted = None Bool
      , cache = None Bool
      , timeout = None Double
      , jobs = None Natural
      , budget = None Double
      , max_oracle_executions = None Natural
      , result_oracle = None Bool
      , oracle = None Text
      , reset = None Text
      , out = None Text
      , in_place = None Bool
      , stats = None Text
      , log_level = None T.LogLevel
      , log_format = None T.LogFormat
      , verbose = None Bool
      , quiet = None Bool
      , color = None T.ColorMode
      }

let dialectText =
      \(d : T.Dialect) ->
        merge
          { postgresql = "postgresql"
          , mysql = "mysql"
          , mariadb = "mariadb"
          , sqlite = "sqlite"
          , duckdb = "duckdb"
          , clickhouse = "clickhouse"
          }
          d

let strategyText =
      \(s : T.Strategy) ->
        merge
          { full = "full"
          , ddmin-only = "ddmin-only"
          , audit-only = "audit-only"
          }
          s

let statementOrderText =
      \(o : T.StatementOrder) ->
        merge
          { ranked-fallback = "ranked-fallback"
          , ranked = "ranked"
          , byte-weighted = "byte-weighted"
          , complement-first = "complement-first"
          , subset-first = "subset-first"
          }
          o

let columnModeText =
      \(m : T.ColumnMode) ->
        merge { off = "off", atomic = "atomic" } m

let optTextFlag =
      \(flag : Text) ->
      \(v : Optional Text) ->
        merge { None = [] : List Text, Some = \(x : Text) -> [ flag, x ] } v

let optBoolFlag =
      \(yesFlag : Text) ->
      \(noFlag : Text) ->
      \(v : Optional Bool) ->
        merge
          { None = [] : List Text
          , Some = \(b : Bool) -> [ if b then yesFlag else noFlag ]
          }
          v

let optEnumFlag =
      \(a : Type) ->
      \(flag : Text) ->
      \(showFn : a -> Text) ->
      \(v : Optional a) ->
        merge
          { None = [] : List Text, Some = \(x : a) -> [ flag, showFn x ] }
          v

let renderFlags
    : T.OccamConfig -> List Text
    = \(cfg : T.OccamConfig) ->
          optEnumFlag T.Dialect "--dialect" dialectText cfg.dialect
        # optEnumFlag T.Strategy "--strategy" strategyText cfg.strategy
        # optBoolFlag "--slicing" "--no-slicing" cfg.slicing
        # optBoolFlag "--closure" "--no-closure" cfg.closure
        # optBoolFlag
            "--intra-statement"
            "--no-intra-statement"
            cfg.intra_statement
        # optEnumFlag
            T.StatementOrder
            "--statement-order"
            statementOrderText
            cfg.statement_order
        # optBoolFlag
            "--literal-batches"
            "--no-literal-batches"
            cfg.literal_batches
        # optBoolFlag "--ast-batching" "--no-ast-batching" cfg.ast_batching
        # optBoolFlag
            "--adaptive-values"
            "--no-adaptive-values"
            cfg.adaptive_values
        # optEnumFlag T.ColumnMode "--column-mode" columnModeText cfg.column_mode
        # optBoolFlag "--unrestricted" "--no-unrestricted" cfg.unrestricted
        # optBoolFlag "--cache" "--no-cache" cfg.cache
        # optTextFlag "--oracle" cfg.oracle
        # optTextFlag "--reset" cfg.reset
        # optTextFlag "--out" cfg.out
        # optTextFlag "--stats" cfg.stats

let mkProfile =
      \(name : Text) ->
      \(key : Text) ->
      \(cfg : T.OccamConfig) ->
        { name = name
        , key = key
        , config = cfg
        , cli_flags = renderFlags cfg
        }
        : T.AblationProfile

in  { defaults, emptyConfig, renderFlags, mkProfile }
