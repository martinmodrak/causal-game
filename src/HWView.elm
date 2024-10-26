module HWView exposing (..)

import Association
import Base64
import Browser
import Game
import GameState exposing (GameState)
import Homework
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Decode
import Maybe.Extra
import Platform.Cmd as Cmd
import Round
import SingleRelationship
import ThreeWay
import TwoRelationships


type alias HWRecord =
    { name : String
    , group : String
    , state : GameState
    }


type alias RecordInfo =
    { filename : String
    , eval : Homework.Eval
    }


type alias Index =
    { updated : String
    , files : List String
    }


type Showing
    = All
    | Single HWRecord


type alias Model =
    { index : Maybe (Result Http.Error Index)
    , records : List ( RecordInfo, HWRecord )
    , errors : List ( String, Http.Error )
    , showing : Showing
    }


type Msg
    = IndexLoaded (Result Http.Error Index)
    | RecordLoaded String (Result Http.Error HWRecord)
    | ShowIndex
    | ShowRecord HWRecord
    | Noop


indexDecoder : Json.Decode.Decoder Index
indexDecoder =
    Json.Decode.map2 Index
        (Json.Decode.field "updated" Json.Decode.string)
        (Json.Decode.field "files"
            (Json.Decode.map Maybe.Extra.values
                (Json.Decode.list
                    (Json.Decode.nullable Json.Decode.string)
                )
            )
        )


base64StringDecoder : Json.Decode.Decoder String
base64StringDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\x ->
                case Base64.decode x of
                    Ok byteList ->
                        byteList
                            |> List.map Char.fromCode
                            |> String.fromList
                            |> Json.Decode.succeed

                    Err err ->
                        Json.Decode.fail err
            )


hwRecordDecoder : Json.Decode.Decoder HWRecord
hwRecordDecoder =
    Json.Decode.map3 HWRecord
        (Json.Decode.oneOf
            [ Json.Decode.field "base64name" base64StringDecoder
            , Json.Decode.field "name" Json.Decode.string
            ]
        )
        (Json.Decode.oneOf
            [ Json.Decode.field "base64group" base64StringDecoder
            , Json.Decode.field "group" Json.Decode.string
            ]
        )
        (Json.Decode.field "state" GameState.gameDecoder)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { index = Nothing
      , records = []
      , errors = []
      , showing = All
      }
    , Http.get
        { url = "hw_data/index.json"
        , expect = Http.expectJson IndexLoaded indexDecoder
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IndexLoaded indexRes ->
            ( { model | index = Just indexRes }
            , case indexRes of
                Ok index ->
                    Cmd.batch
                        (List.map
                            (\filename ->
                                Http.get
                                    { url = "hw_data/" ++ filename
                                    , expect = Http.expectJson (RecordLoaded filename) hwRecordDecoder
                                    }
                            )
                            index.files
                        )

                Err _ ->
                    Cmd.none
            )

        RecordLoaded filename res ->
            case res of
                Ok rec ->
                    let
                        info =
                            { filename = filename
                            , eval = Homework.computeScore (Game.getResults TwoRelationships.adapter.logic rec.state.twoRel.history)
                            }
                    in
                    ( { model | records = ( info, rec ) :: model.records }, Cmd.none )

                Err err ->
                    ( { model | errors = ( filename, err ) :: model.errors }, Cmd.none )

        ShowIndex ->
            ( { model | showing = All }, Cmd.none )

        ShowRecord rec ->
            ( { model | showing = Single rec }, Cmd.none )

        Noop ->
            ( model, Cmd.none )


suppressMsg : Html a -> Html Msg
suppressMsg html =
    Html.map (always Noop) html


view : Model -> Html Msg
view model =
    case model.showing of
        All ->
            case model.index of
                Just (Ok _) ->
                    viewIndex model

                Just (Err err) ->
                    viewHttpError err

                Nothing ->
                    text "Loading..."

        Single rec ->
            viewSingleRec rec


viewHttpError : Http.Error -> Html a
viewHttpError err =
    case err of
        Http.BadBody errText ->
            text errText

        _ ->
            Tuple.first ( text "Other error", Debug.log "Err" err )


viewIndex : Model -> Html Msg
viewIndex model =
    let
        singleRec =
            \( info, rec ) ->
                tr []
                    [ td [] [ text rec.name ]
                    , td [] [ text rec.group ]
                    , td [] [ text (String.fromInt info.eval.score) ]
                    , td [] [ text (Round.round 0 (info.eval.avgCorrect * 100) ++ "%") ]
                    , td [] [ text (Round.round 0 (info.eval.avgCost / 1000) ++ "K") ]
                    , td [] [ text info.filename ]
                    , td [] [ a [ Attr.href "#", Events.onClick (ShowRecord rec) ] [ text "View" ] ]
                    ]
    in
    div []
        [ viewErrors model.errors
        , table []
            (tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Group" ]
                , th [] [ text "Score" ]
                , th [] [ text "Correct" ]
                , th [] [ text "Cost" ]
                , th [] [ text "Filename" ]
                , th [] [ text "View" ]
                ]
                :: List.map singleRec model.records
            )
        ]


viewErrors : List ( String, Http.Error ) -> Html a
viewErrors errors =
    let
        viewErr =
            \( file, err ) -> li [] [ strong [] [ text file ], viewHttpError err ]
    in
    if List.isEmpty errors then
        text ""

    else
        div []
            [ h2 [] [ text "There were errors" ]
            , ul [] (List.map viewErr errors)
            ]


viewSingleRec : HWRecord -> Html Msg
viewSingleRec rec =
    div [ Attr.class "topContainer" ]
        [ p []
            [ button [ Attr.type_ "_button", Events.onClick ShowIndex ] [ text "Back to list" ]
            , strong [] [ text "Name: " ]
            , text rec.name
            , text " "
            , strong [] [ text "Group: " ]
            , text rec.group
            ]
        , div
            [ Attr.class "scenarioPage" ]
            [ suppressMsg (Game.view Association.adapter rec.state.association)
            ]
        , div [ Attr.class "scenarioPage" ]
            [ suppressMsg (Game.view SingleRelationship.adapter rec.state.singleRel)
            ]
        , div [ Attr.class "scenarioPage" ]
            [ suppressMsg (Game.view TwoRelationships.adapter rec.state.twoRel)
            ]
        , div [ Attr.class "scenarioPage" ]
            [ suppressMsg (Game.view ThreeWay.adapter rec.state.threeWay)
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
