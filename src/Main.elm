port module Main exposing (..)

import Association
import Bitwise exposing (and)
import Browser
import Game
import GameState
import Homework
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy
import Instructions
import Json.Decode
import Json.Encode
import Settings
import SingleRelationship
import ThreeWay
import TwoRelationships


type Page
    = InstructionsPage
    | AssocPage
    | SingleRelPage
    | TwoRelPage
    | ThreeWayPage


type alias StoredModel =
    { page : Page
    , game : GameState.GameState
    }


type alias Model =
    { page : Page
    , game : GameState.GameState
    , storedModel : Maybe StoredModel
    , homework : Homework.Model
    , viewSettings : Game.ViewSettings
    }


type Msg
    = AssocMsg Association.Msg
    | SingleRel SingleRelationship.Msg
    | TwoRel TwoRelationships.Msg
    | ThreeWay ThreeWay.Msg
    | ActivatePage Page
    | LoadStored
    | DismissStored
    | Homework Homework.Msg


port setStorage : Json.Encode.Value -> Cmd msg


init : Json.Encode.Value -> ( Model, Cmd Msg )
init storedModel =
    ( { page = InstructionsPage
      , game =
            { association = Game.init Association.adapter
            , singleRel = Game.init SingleRelationship.adapter
            , twoRel = Game.init TwoRelationships.adapter
            , threeWay = Game.init ThreeWay.adapter
            }
      , storedModel =
            case Json.Decode.decodeValue storedModelDecoder storedModel of
                Ok stored ->
                    if GameState.gameHasData stored.game then
                        Just stored

                    else
                        Nothing

                Err _ ->
                    Nothing
      , homework = Homework.init
      , viewSettings = Game.DotPlot
      }
    , Cmd.batch
        [ Cmd.map AssocMsg Game.initCmd
        , Cmd.map SingleRel Game.initCmd
        , Cmd.map TwoRel Game.initCmd
        , Cmd.map ThreeWay Game.initCmd
        ]
    )


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , updateStorage : Bool
    }


update : Msg -> Model -> UpdateResult
update msg model =
    let
        game =
            model.game
    in
    case msg of
        AssocMsg assocMsg ->
            let
                res =
                    Game.update model.viewSettings Association.adapter assocMsg model.game.association
            in
            UpdateResult { model | game = { game | association = res.scenario }, viewSettings = res.viewSettings } (Cmd.map AssocMsg res.cmd) res.updateStorage

        SingleRel singleMsg ->
            let
                res =
                    Game.update model.viewSettings SingleRelationship.adapter singleMsg model.game.singleRel
            in
            UpdateResult { model | game = { game | singleRel = res.scenario }, viewSettings = res.viewSettings }
                (Cmd.map SingleRel res.cmd)
                res.updateStorage

        TwoRel twoWayMsg ->
            let
                res =
                    Game.update model.viewSettings TwoRelationships.adapter twoWayMsg model.game.twoRel
            in
            UpdateResult { model | game = { game | twoRel = res.scenario }, viewSettings = res.viewSettings } (Cmd.map TwoRel res.cmd) res.updateStorage

        ThreeWay subMsg ->
            let
                res =
                    Game.update model.viewSettings ThreeWay.adapter subMsg model.game.threeWay
            in
            UpdateResult { model | game = { game | threeWay = res.scenario }, viewSettings = res.viewSettings } (Cmd.map ThreeWay res.cmd) res.updateStorage

        ActivatePage page ->
            UpdateResult { model | page = page } Cmd.none False

        LoadStored ->
            case model.storedModel of
                Just stored ->
                    UpdateResult { model | page = stored.page, game = stored.game, storedModel = Nothing } Cmd.none False

                Nothing ->
                    UpdateResult model Cmd.none False

        DismissStored ->
            UpdateResult { model | storedModel = Nothing } Cmd.none False

        Homework subMsg ->
            let
                ( newModel, newCmd ) =
                    Homework.update subMsg model.game model.homework
            in
            UpdateResult { model | homework = newModel } (Cmd.map Homework newCmd) False


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        res =
            update msg oldModel
    in
    ( res.model
    , case oldModel.storedModel of
        Just _ ->
            res.cmd

        Nothing ->
            if res.updateStorage && GameState.gameHasData res.model.game then
                Cmd.batch [ setStorage (storedModelEncoder { page = res.model.page, game = res.model.game }), res.cmd ]

            else
                res.cmd
    )


nextScenarioButton : Page -> Html Msg
nextScenarioButton page =
    button [ Attr.type_ "input", Events.onClick (ActivatePage page), Attr.class "glow" ] [ text "Start next scenario" ]


view : Model -> Html Msg
view model =
    let
        homeworkControls =
            if Settings.homeworkEnabled then
                Html.map Homework (Homework.viewControls model.game)

            else
                text ""
    in
    div [ Attr.class "topContainer" ]
        [ if Settings.displayTruth then
            div [ Attr.style "font-size" "4em", Attr.style "color" "red" ] [ text "Showing truth!" ]

          else
            text ""
        , Html.Lazy.lazy viewPageSelection model
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, InstructionsPage ) ( "block", "none" )) ]
            [ Html.map never Instructions.view
            , p []
                [ button [ Attr.type_ "input", Events.onClick (ActivatePage AssocPage) ] [ text "Start the first scenario!" ]
                ]
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, AssocPage ) ( "block", "none" )) ]
            [ Game.view model.viewSettings Association.adapter model.game.association (text "") (Just (nextScenarioButton SingleRelPage)) AssocMsg
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, SingleRelPage ) ( "block", "none" )) ]
            [ Game.view model.viewSettings SingleRelationship.adapter model.game.singleRel (text "") (Just (nextScenarioButton TwoRelPage)) SingleRel
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, TwoRelPage ) ( "block", "none" )) ]
            [ Game.view model.viewSettings TwoRelationships.adapter model.game.twoRel homeworkControls Nothing TwoRel
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, ThreeWayPage ) ( "block", "none" )) ]
            [ Game.view model.viewSettings ThreeWay.adapter model.game.threeWay (text "") Nothing ThreeWay
            ]
        , case model.storedModel of
            Just _ ->
                viewStoredPopup

            Nothing ->
                if model.homework.submission && Settings.homeworkEnabled then
                    Html.map Homework (Homework.viewPopup model.game model.homework)

                else
                    text ""
        ]


viewStoredPopup : Html Msg
viewStoredPopup =
    div [ Attr.class "popupBackground" ]
        [ div [ Attr.class "popup" ]
            [ p []
                [ text "Found stored data from previous game." ]
            , p []
                [ strong [] [ text "Do you wish to continue your previously started game?" ]
                ]
            , button [ Attr.class "left", Attr.type_ "button", Events.onClick LoadStored ] [ strong [] [ text "Continue game" ] ]
            , button [ Attr.class "right", Attr.type_ "button", Events.onClick DismissStored ] [ text "Start fresh" ]
            ]
        ]


viewPageSelection : Model -> Html Msg
viewPageSelection model =
    div [ Attr.class "pageSelectionBox" ] (List.map (viewPageSelectionButton model.page) [ InstructionsPage, AssocPage, SingleRelPage, TwoRelPage, ThreeWayPage ])


viewPageSelectionButton : Page -> Page -> Html Msg
viewPageSelectionButton activePage page =
    ifActive ( activePage, page )
        ( h2 [ Attr.class "pageSelection", Attr.class "active" ] [ text (pageTitle page) ]
        , h2 [ Attr.class "pageSelection", Attr.class "inactive" ] [ a [ Events.onClick (ActivatePage page) ] [ text (pageTitle page) ] ]
        )


pageTitle : Page -> String
pageTitle page =
    case page of
        InstructionsPage ->
            "Instructions"

        AssocPage ->
            "1: Association"

        SingleRelPage ->
            "2: Cause"

        TwoRelPage ->
            if Settings.homeworkEnabled then
                "Homework"

            else
                "3: Two causes"

        ThreeWayPage ->
            if Settings.homeworkEnabled then
                "Bonus"

            else
                "4: Three causes"


ifActive : ( Page, Page ) -> ( a, a ) -> a
ifActive ( activePage, page ) ( activeOutput, inactiveOutput ) =
    if page == activePage then
        activeOutput

    else
        inactiveOutput


pageDecoder : Json.Decode.Decoder Page
pageDecoder =
    let
        stringToPage s =
            case s of
                "Instr" ->
                    InstructionsPage

                "Assoc" ->
                    AssocPage

                "SingleRel" ->
                    SingleRelPage

                "TwoRel" ->
                    TwoRelPage

                "ThreeWay" ->
                    ThreeWayPage

                _ ->
                    InstructionsPage
    in
    Json.Decode.string
        |> Json.Decode.map stringToPage


pageEncoder : Page -> Json.Encode.Value
pageEncoder page =
    let
        stringRepr =
            case page of
                InstructionsPage ->
                    "Instr"

                AssocPage ->
                    "Assoc"

                SingleRelPage ->
                    "SingleRel"

                TwoRelPage ->
                    "TwoRel"

                ThreeWayPage ->
                    "ThreeWay"
    in
    Json.Encode.string stringRepr


storedModelDecoder : Json.Decode.Decoder StoredModel
storedModelDecoder =
    Json.Decode.map2 StoredModel
        (Json.Decode.field "page" pageDecoder)
        (Json.Decode.field "game" GameState.gameDecoder)


storedModelEncoder : StoredModel -> Json.Encode.Value
storedModelEncoder sm =
    Json.Encode.object
        [ ( "page", pageEncoder sm.page )
        , ( "game", GameState.gameEncoder sm.game )
        ]


main : Program Json.Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = always Sub.none
        }
