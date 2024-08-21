module Main exposing (..)

import Associations exposing (associationSpecGenerator)
import Browser
import Random
import Types exposing (..)
import Update exposing (update)
import View exposing (view)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { association = { scenarios = [], proposedExperiment = 20 } }
    , Cmd.map AssocMsg (Random.generate AssocSpecGenerated associationSpecGenerator)
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
