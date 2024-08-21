module Types exposing (..)

import VegaLite


type alias AssociationSpec =
    { slope : Float
    , intercept : Float
    , noise : Float
    }


type alias AssociationExperiment =
    Int


type alias AssociationScenario =
    { spec : AssociationSpec
    , data : List ( AssociationExperiment, VegaLite.Data )
    }


type alias AssociationModel =
    { scenarios : List AssociationScenario
    , proposedExperiment : AssociationExperiment
    }


type alias Model =
    { association : AssociationModel
    }


type AssociationMsg
    = AssocSpecGenerated AssociationSpec
    | AssocRunExperiment
    | AssocData AssociationExperiment VegaLite.Data
    | AssocSetN String


type Msg
    = AssocMsg AssociationMsg
