module Main exposing (..)

import Browser
import Generators exposing (associationSpecGenerator)
import Random
import Types exposing (Model, Msg(..), Scenario(..))
import Update exposing (update)
import View exposing (view)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { scenarios = [] }
    , Random.generate ScenarioGenerated (Random.map Association associationSpecGenerator)
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
