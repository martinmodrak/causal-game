module Main exposing (..)

import Association
import Browser
import Game
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events


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
        [ Html.map AssocMsg (Game.view Association.adapter model.association) ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
