-- Composable RQ1-RQ4 Occam ablation configs and rendered CLI profiles.
let T = ./Types.dhall

let O = ./OccamDefaults.dhall

let auditOnlyConfig
    : T.OccamConfig
    = O.emptyConfig // { strategy = Some T.Strategy.audit-only }

let ddminOnlyConfig
    : T.OccamConfig
    = O.emptyConfig // { strategy = Some T.Strategy.ddmin-only }

let oursSConfig
    : T.OccamConfig
    = O.emptyConfig
        // { strategy = Some T.Strategy.full, intra_statement = Some False }

let oursSIConfig
    : T.OccamConfig
    = O.emptyConfig
        // { strategy = Some T.Strategy.full
           , intra_statement = Some True
           , column_mode = Some T.ColumnMode.off
           }

let oursFullConfig
    : T.OccamConfig
    = O.emptyConfig
        // { strategy = Some T.Strategy.full
           , intra_statement = Some True
           , column_mode = Some T.ColumnMode.atomic
           }

let oursNoDepConfig
    : T.OccamConfig
    = O.emptyConfig
        // { strategy = Some T.Strategy.full
           , slicing = Some False
           , closure = Some False
           }

let profiles
    : List T.AblationProfile
    = [ O.mkProfile "audit-only" "audit-only" auditOnlyConfig
      , O.mkProfile "ddmin-only" "ddmin-only" ddminOnlyConfig
      , O.mkProfile "Ours-S" "ours-s" oursSConfig
      , O.mkProfile "Ours-SI" "ours-si" oursSIConfig
      , O.mkProfile "Ours-Full" "ours-full" oursFullConfig
      , O.mkProfile "Ours-NoDep" "ours-nodep" oursNoDepConfig
      ]

in  { auditOnlyConfig
    , ddminOnlyConfig
    , oursSConfig
    , oursSIConfig
    , oursFullConfig
    , oursNoDepConfig
    , profiles
    }
