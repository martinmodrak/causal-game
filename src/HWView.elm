module HWView exposing (..)

import Association
import Base64
import Base64String
import Browser
import Bytes.Encode
import Game
import GameJson
import GameState exposing (GameState)
import Homework
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Decode
import Json.Encode
import Maybe.Extra
import MultiBiDict
import Platform.Cmd as Cmd
import Round
import Set
import SingleRelationship
import ThreeWay
import TwoRelationships


type alias HWRecord =
    { name : String
    , group : String
    , state : GameState
    }


type alias FileInfo =
    { name : String
    , dir : String
    , time : String
    }


type alias RecordInfo =
    { file : FileInfo
    , eval : Homework.Eval
    }


type alias Index =
    { updated : String
    , files : List FileInfo
    }


type Showing
    = All
    | Single HWRecord


type LoadState
    = WaitingForIndex
    | WaitingForRecrods Int
    | Loaded String


type alias InstanceDict =
    --first key is instance
    MultiBiDict.MultiBiDict String String


type alias Model =
    { index : Maybe (Result Http.Error Index)
    , recordsToLoad : Int
    , records : List ( RecordInfo, HWRecord )
    , errors : List ( FileInfo, Http.Error )
    , showing : Showing
    , csvDownload : Maybe String
    , instanceDict : InstanceDict
    }


type Msg
    = IndexLoaded (Result Http.Error Index)
    | RecordLoaded FileInfo (Result Http.Error HWRecord)
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
                    (Json.Decode.nullable
                        (Json.Decode.map3 FileInfo
                            (Json.Decode.field "name" Json.Decode.string)
                            (Json.Decode.field "dir" Json.Decode.string)
                            (Json.Decode.field "time" Json.Decode.string)
                        )
                    )
                )
            )
        )


base64StringDecoder : Json.Decode.Decoder String
base64StringDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\x ->
                case Base64String.decode x of
                    Ok res ->
                        Json.Decode.succeed res

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
      , recordsToLoad = 0
      , records = []
      , errors = []
      , showing = All
      , csvDownload = Nothing
      , instanceDict = MultiBiDict.empty
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
            let
                ( toLoad, cmd ) =
                    case indexRes of
                        Ok index ->
                            ( List.length index.files
                            , Cmd.batch
                                (List.map
                                    (\file ->
                                        Http.get
                                            { url = file.dir ++ "/" ++ file.name
                                            , expect = Http.expectJson (RecordLoaded file) hwRecordDecoder
                                            }
                                    )
                                    index.files
                                )
                            )

                        Err _ ->
                            ( 0, Cmd.none )
            in
            ( { model
                | index = Just indexRes
                , recordsToLoad = toLoad
              }
            , cmd
            )

        RecordLoaded file res ->
            let
                modelAfterLoad =
                    case res of
                        Ok rec ->
                            let
                                info =
                                    { file = file
                                    , eval = Homework.computeScore rec.state --(Game.getResults TwoRelationships.adapter.logic rec.state.twoRel.history)
                                    }
                            in
                            { model
                                | records = ( info, rec ) :: model.records
                                , recordsToLoad = model.recordsToLoad - 1
                                , instanceDict = updateInstanceDict rec.name rec.state model.instanceDict
                            }

                        Err err ->
                            { model | errors = ( file, err ) :: model.errors, recordsToLoad = model.recordsToLoad - 1 }
            in
            ( modelAfterLoad |> updateDownload, Cmd.none )

        ShowIndex ->
            ( { model | showing = All }, Cmd.none )

        ShowRecord rec ->
            ( { model | showing = Single rec }, Cmd.none )

        Noop ->
            ( model, Cmd.none )


updateInstanceDict : String -> GameState -> InstanceDict -> InstanceDict
updateInstanceDict name state old =
    old
        |> updateInstanceDictSingle name GameJson.associationInstanceEncoder state.association
        |> updateInstanceDictSingle name GameJson.singleRelInstanceEncoder state.singleRel
        |> updateInstanceDictSingle name GameJson.twoRelInstanceEncoder state.twoRel
        |> updateInstanceDictSingle name GameJson.threeWayInstanceEncoder state.threeWay


updateInstanceDictSingle : String -> (Game.Instance spec experiment outcome guess -> Json.Encode.Value) -> Game.Scenario spec experiment outcome guess -> InstanceDict -> InstanceDict
updateInstanceDictSingle name encoder scenario old =
    let
        instanceToString =
            encoder >> Json.Encode.encode 0

        processItem =
            \item dict ->
                MultiBiDict.insert (instanceToString item) name dict
    in
    List.foldl processItem old scenario.history


updateDownload : Model -> Model
updateDownload model =
    case model.index of
        Nothing ->
            model

        Just _ ->
            if model.recordsToLoad <= 0 then
                { model | csvDownload = Just (buildCSV model.records) }

            else
                model


buildCSV : List ( RecordInfo, HWRecord ) -> String
buildCSV records =
    let
        base64Content =
            Bytes.Encode.sequence
                (Bytes.Encode.string "name,lang,score\n"
                    :: List.map (singleCSVLine >> Bytes.Encode.string) records
                )
                |> Bytes.Encode.encode
                |> Base64.fromBytes
                |> Maybe.withDefault "RXJyb3IgNTYyNw=="
    in
    "data:application/json;base64," ++ base64Content


singleCSVLine : ( RecordInfo, HWRecord ) -> String
singleCSVLine ( info, rec ) =
    "\""
        ++ rec.name
        ++ "\",\""
        ++ String.replace "hw_data/" "" info.file.dir
        ++ "\","
        ++ String.fromInt info.eval.score
        ++ "\n"


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
            viewRecordDetail rec


viewHttpError : Http.Error -> Html a
viewHttpError err =
    case err of
        Http.BadBody errText ->
            text errText

        _ ->
            Tuple.first ( text "Other error", Debug.log "Err" err )


viewIndex : Model -> Html Msg
viewIndex model =
    div []
        [ p []
            [ if model.recordsToLoad > 0 then
                strong [] [ text ("Waiting for " ++ String.fromInt model.recordsToLoad ++ " records.") ]

              else
                case
                    model.csvDownload
                of
                    Nothing ->
                        text "Building CSV"

                    Just csv ->
                        a [ Attr.href csv, Attr.download "homework_causality.csv" ] [ text "Download CSV" ]
            ]
        , viewErrors model.errors
        , table []
            (tr []
                [ th [] [ text "Dir" ]
                , th [] [ text "Name" ]
                , th [] [ text "Group" ]
                , th [] [ text "Score" ]
                , th [] [ text "Correct" ]
                , th [] [ text "Cost" ]
                , th [] [ text "Filename" ]
                , th [] [ text "Overlaps" ]
                , th [] [ text "View" ]
                ]
                :: List.map (viewSingleRec (model.recordsToLoad == 0) model.instanceDict) model.records
            )
        ]


viewSingleRec : Bool -> InstanceDict -> ( RecordInfo, HWRecord ) -> Html Msg
viewSingleRec allLoaded instanceDict ( info, rec ) =
    let
        overlaps =
            if allLoaded then
                getOverlaps instanceDict rec.name

            else
                []
    in
    tr []
        [ td [] [ text info.file.dir ]
        , td [] [ text rec.name ]
        , td [] [ text rec.group ]
        , td [] [ text (String.fromInt info.eval.score) ]
        , td [] [ text (Round.round 0 (info.eval.avgCorrect * 100) ++ "% of " ++ String.fromInt info.eval.nInstances) ]
        , td [] [ text (Round.round 0 (info.eval.avgCost / 1000) ++ "K") ]
        , td [] [ text info.file.name ]
        , td [] (List.map (\x -> span [] [ text x, text ", " ]) overlaps)
        , td [] [ a [ Attr.href "#", Events.onClick (ShowRecord rec) ] [ text "View" ] ]
        ]


getOverlaps : InstanceDict -> String -> List String
getOverlaps instanceDict name =
    MultiBiDict.getReverse name instanceDict
        |> Set.toList
        |> List.map (\inst -> MultiBiDict.get inst instanceDict)
        |> List.foldl Set.union Set.empty
        |> Set.remove name
        |> Set.toList


viewErrors : List ( FileInfo, Http.Error ) -> Html a
viewErrors errors =
    let
        viewErr =
            \( file, err ) -> li [] [ strong [] [ text (file.dir ++ "/" ++ file.name) ], viewHttpError err ]
    in
    if List.isEmpty errors then
        text ""

    else
        div []
            [ h2 [] [ text "There were errors" ]
            , ul [] (List.map viewErr errors)
            ]


viewRecordDetail : HWRecord -> Html Msg
viewRecordDetail rec =
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
            [ suppressMsg (Game.view Game.Both Association.adapter rec.state.association)
            ]
        , div [ Attr.class "scenarioPage" ]
            [ suppressMsg (Game.view Game.Both SingleRelationship.adapter rec.state.singleRel)
            ]
        , div [ Attr.class "scenarioPage" ]
            [ suppressMsg (Game.view Game.Both TwoRelationships.adapter rec.state.twoRel)
            ]
        , div [ Attr.class "scenarioPage" ]
            [ suppressMsg (Game.view Game.Both ThreeWay.adapter rec.state.threeWay)
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
