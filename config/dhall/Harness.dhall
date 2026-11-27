-- Unified harness configuration compiled to config/generated/harness.json.
let O = ./OccamDefaults.dhall

let D = ./DbmsSpecs.dhall

let P = ./Profiles.dhall

let S = ./Squirrel.dhall

let E = ./Experiment.dhall

in  { occam_defaults = O.defaults
    , dbms = D
    , profiles = P.profiles
    , squirrel = S
    , oracle = E.oracleDefaults
    , plans = { smoke = E.smokePlan, stage2_thesis = E.stage2ThesisPlan }
    }
