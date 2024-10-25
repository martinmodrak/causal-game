module HWView exposing (..)

import Association
import Browser
import Game
import GameState exposing (GameState)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Decode
import SingleRelationship
import ThreeWay
import TwoRelationships


type alias HWRecord =
    { name : String
    , group : String
    , state : GameState
    }


type alias Model =
    { record : Maybe HWRecord
    }


type Msg
    = RecordLoaded (Result Http.Error HWRecord)
    | Noop


hwRecordDecoder : Json.Decode.Decoder HWRecord
hwRecordDecoder =
    Json.Decode.map3 HWRecord
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "group" Json.Decode.string)
        (Json.Decode.field "state" GameState.gameDecoder)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { record = Nothing }
    , Http.get
        { url = "./hw_data/homework.json"
        , expect = Http.expectJson RecordLoaded hwRecordDecoder
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        RecordLoaded res ->
            case res of
                Ok rec ->
                    { model | record = Just rec }

                Err err ->
                    Tuple.second ( Debug.log "err" err, model )

        Noop ->
            model
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    case model.record of
        Just rec ->
            div [ Attr.class "topContainer" ]
                [ p []
                    [ strong [] [ text "Name: " ], text rec.name, text " ", strong [] [ text "Group: " ], text rec.group ]
                , div
                    [ Attr.class "scenarioPage" ]
                    [ Html.map (always Noop) (Game.view Association.adapter rec.state.association)
                    ]
                , div [ Attr.class "scenarioPage" ]
                    [ Html.map (always Noop) (Game.view SingleRelationship.adapter rec.state.singleRel)
                    ]
                , div [ Attr.class "scenarioPage" ]
                    [ Html.map (always Noop) (Game.view TwoRelationships.adapter rec.state.twoRel)
                    ]
                , div [ Attr.class "scenarioPage" ]
                    [ Html.map (always Noop) (Game.view ThreeWay.adapter rec.state.threeWay)
                    ]
                ]

        Nothing ->
            text "Loading..."


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
