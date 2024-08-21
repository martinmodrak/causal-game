module Types exposing (..)

import VegaLite


type Scenario
    = Association AssociationSpec


type alias AssociationSpec =
    { slope : Float
    , intercept : Float
    , noise : Float
    }


type alias ScenarioModel =
    { scenario : Scenario
    , data : List VegaLite.Data
    }


type alias Model =
    { scenarios : List ScenarioModel
    }


type Msg
    = ScenarioGenerated Scenario
    | GetData Scenario
    | DataGenerated VegaLite.Data
