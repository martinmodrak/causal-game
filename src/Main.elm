port module Main exposing (..)

import Association
import Base64
import Browser
import Causality
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
import Round
import SingleRelationship
import ThreeWay
import TwoRelationships
import UnobservedConfounding
import Utils


type Page
    = InstructionsPage
    | AssocPage
    | SingleRelPage
    | TwoRelPage
    | ThreeWayPage


type alias Model =
    { page : Page
    , game : GameState.GameState
    , storedGame : Maybe GameState.GameState
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
init storedGame =
    ( { page = InstructionsPage
      , game =
            { association = Game.init Association.adapter
            , singleRel = Game.init SingleRelationship.adapter
            , twoRel = Game.init TwoRelationships.adapter
            , threeWay = Game.init ThreeWay.adapter
            }
      , storedGame =
            case Json.Decode.decodeValue GameState.gameDecoder storedGame of
                Ok stored ->
                    if GameState.gameHasData stored then
                        Just stored

                    else
                        Nothing

                Err _ ->
                    Nothing
      , homework = Homework.init
      , viewSettings = Game.DotPlot
      }
    , Cmd.batch
        [ Cmd.map AssocMsg (Game.initCmd Association.adapter)
        , Cmd.map SingleRel (Game.initCmd SingleRelationship.adapter)
        , Cmd.map TwoRel (Game.initCmd TwoRelationships.adapter)
        , Cmd.map ThreeWay (Game.initCmd ThreeWay.adapter)
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
            case model.storedGame of
                Just stored ->
                    UpdateResult { model | game = stored, storedGame = Nothing } Cmd.none False

                Nothing ->
                    UpdateResult model Cmd.none False

        DismissStored ->
            UpdateResult { model | storedGame = Nothing } Cmd.none False

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
    , case oldModel.storedGame of
        Just _ ->
            res.cmd

        Nothing ->
            if res.updateStorage && GameState.gameHasData res.model.game then
                Cmd.batch [ setStorage (GameState.gameEncoder res.model.game), res.cmd ]

            else
                res.cmd
    )


view : Model -> Html Msg
view model =
    div [ Attr.class "topContainer" ]
        [ Html.map Homework (Homework.viewControls model.game)
        , Html.Lazy.lazy viewPageSelection model
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, InstructionsPage ) ( "block", "none" )) ]
            [ Html.map never Instructions.view
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, AssocPage ) ( "block", "none" )) ]
            [ Html.map AssocMsg (Game.view model.viewSettings Association.adapter model.game.association)
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, SingleRelPage ) ( "block", "none" )) ]
            [ Html.map SingleRel (Game.view model.viewSettings SingleRelationship.adapter model.game.singleRel)
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, TwoRelPage ) ( "block", "none" )) ]
            [ Html.map TwoRel (Game.view model.viewSettings TwoRelationships.adapter model.game.twoRel)
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, ThreeWayPage ) ( "block", "none" )) ]
            [ Html.map ThreeWay (Game.view model.viewSettings ThreeWay.adapter model.game.threeWay)
            ]
        , case model.storedGame of
            Just _ ->
                viewStoredPopup

            Nothing ->
                if model.homework.submission then
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
    div [] (List.map (viewPageSelectionButton model.page) [ InstructionsPage, AssocPage, SingleRelPage, TwoRelPage, ThreeWayPage ])


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
            "0: Association"

        SingleRelPage ->
            "1: Cause"

        TwoRelPage ->
            "Homework"

        ThreeWayPage ->
            "Bonus"


ifActive : ( Page, Page ) -> ( a, a ) -> a
ifActive ( activePage, page ) ( activeOutput, inactiveOutput ) =
    if page == activePage then
        activeOutput

    else
        inactiveOutput


main : Program Json.Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = always Sub.none
        }
