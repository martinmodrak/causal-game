module Main exposing (..)

import Association
import Browser
import Causality exposing (outcomeToSpec)
import Game
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import VegaLite as VL
import View exposing (vegaPlot)


type alias Model =
    { association : Association.Model
    }


type Msg
    = AssocMsg Association.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { association = Game.init Association.adapter }
    , Cmd.map AssocMsg (Game.initCmd Association.adapter)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AssocMsg assocMsg ->
            let
                ( newAssocModel, assocCmd ) =
                    Game.update Association.adapter assocMsg model.association
            in
            ( { model | association = newAssocModel }, Cmd.map AssocMsg assocCmd )


view : Model -> Html Msg
view model =
    div []
        [ Html.map AssocMsg (Game.view Association.adapter model.association)
        ]


meanInner : List Bool -> Int -> Int -> Float
meanInner remaining total length =
    case remaining of
        [] ->
            toFloat total / toFloat length

        x :: rest ->
            let
                inc =
                    if x then
                        1

                    else
                        0
            in
            meanInner rest (total + inc) (length + 1)


mean : List Bool -> Float
mean list =
    case list of
        [] ->
            -1

        True :: xs ->
            meanInner xs 1 1

        False :: xs ->
            meanInner xs 0 1


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
