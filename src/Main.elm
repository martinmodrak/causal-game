module Main exposing (..)

import Association
import Browser
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
    ( { association = Association.initModel }
    , Cmd.map AssocMsg Association.initCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AssocMsg assocMsg ->
            let
                ( newAssocModel, assocCmd ) =
                    Association.update assocMsg model.association
            in
            ( { model | association = newAssocModel }, Cmd.map AssocMsg assocCmd )


view : Model -> Html Msg
view model =
    div []
        [ Html.map AssocMsg (Association.view model.association) ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
