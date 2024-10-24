port module Main exposing (..)

import Association
import Browser
import Game
import GameJson
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import Json.Encode
import SingleRelationship
import TwoRelationships
import UnobservedConfounding


type Page
    = AssocPage
    | SingleRelPage
    | TwoRelPage


type alias GameState =
    { association : Association.Model
    , singleRel : SingleRelationship.Model
    , twoRel : TwoRelationships.Model
    }


type alias Model =
    { page : Page
    , game : GameState
    , storedGame : Maybe GameState
    }


type Msg
    = AssocMsg Association.Msg
    | SingleRel SingleRelationship.Msg
    | TwoRel TwoRelationships.Msg
    | ActivatePage Page
    | LoadStored
    | DismissStored


port setStorage : Json.Encode.Value -> Cmd msg


init : Json.Encode.Value -> ( Model, Cmd Msg )
init storedGame =
    ( { page = AssocPage
      , game =
            { association = Game.init Association.adapter
            , singleRel = Game.init SingleRelationship.adapter
            , twoRel = Game.init TwoRelationships.adapter
            }
      , storedGame =
            case Json.Decode.decodeValue gameDecoder storedGame of
                Ok stored ->
                    if gameHasData stored then
                        Just stored

                    else
                        Nothing

                Err a ->
                    Tuple.first ( Nothing, Debug.log "Err JSON: " a )
      }
    , Cmd.batch
        [ Cmd.map AssocMsg (Game.initCmd Association.adapter)
        , Cmd.map SingleRel (Game.initCmd SingleRelationship.adapter)
        , Cmd.map TwoRel (Game.initCmd TwoRelationships.adapter)
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        game =
            model.game
    in
    case msg of
        AssocMsg assocMsg ->
            let
                ( newAssocModel, assocCmd ) =
                    Game.update Association.adapter assocMsg model.game.association
            in
            ( { model | game = { game | association = newAssocModel } }, Cmd.map AssocMsg assocCmd )

        SingleRel singleMsg ->
            let
                ( newSingleRelModel, singleCmd ) =
                    Game.update SingleRelationship.adapter singleMsg model.game.singleRel
            in
            ( { model | game = { game | singleRel = newSingleRelModel } }, Cmd.map SingleRel singleCmd )

        TwoRel twoWayMsg ->
            let
                ( newTwoWayModel, twoWayCmd ) =
                    Game.update TwoRelationships.adapter twoWayMsg model.game.twoRel
            in
            ( { model | game = { game | twoRel = newTwoWayModel } }, Cmd.map TwoRel twoWayCmd )

        ActivatePage page ->
            ( { model | page = page }, Cmd.none )

        LoadStored ->
            case model.storedGame of
                Just stored ->
                    ( { model | game = stored, storedGame = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        DismissStored ->
            ( { model | storedGame = Nothing }, Cmd.none )


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , case oldModel.storedGame of
        Just _ ->
            cmds

        Nothing ->
            if gameHasData newModel.game then
                Cmd.batch [ setStorage (gameEncoder newModel.game), cmds ]

            else
                cmds
    )


view : Model -> Html Msg
view model =
    div [ Attr.class "topContainer" ]
        [ viewPageSelection model
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, AssocPage ) ( "block", "none" )) ]
            [ Html.map AssocMsg (Game.view Association.adapter model.game.association)
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, SingleRelPage ) ( "block", "none" )) ]
            [ Html.map SingleRel (Game.view SingleRelationship.adapter model.game.singleRel)
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, TwoRelPage ) ( "block", "none" )) ]
            [ Html.map TwoRel (Game.view TwoRelationships.adapter model.game.twoRel)
            ]
        , case model.storedGame of
            Just _ ->
                viewStoredPopup

            Nothing ->
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
    div [] (List.map (viewPageSelectionButton model.page) [ AssocPage, SingleRelPage, TwoRelPage ])


viewPageSelectionButton : Page -> Page -> Html Msg
viewPageSelectionButton activePage page =
    ifActive ( activePage, page )
        ( h2 [ Attr.class "pageSelection", Attr.class "active" ] [ text (pageTitle page) ]
        , h2 [ Attr.class "pageSelection", Attr.class "inactive" ] [ a [ Events.onClick (ActivatePage page) ] [ text (pageTitle page) ] ]
        )


pageTitle : Page -> String
pageTitle page =
    case page of
        AssocPage ->
            "0: Association"

        SingleRelPage ->
            "1: Single relationship"

        TwoRelPage ->
            "2: Two relationships"


ifActive : ( Page, Page ) -> ( a, a ) -> a
ifActive ( activePage, page ) ( activeOutput, inactiveOutput ) =
    if page == activePage then
        activeOutput

    else
        inactiveOutput


gameEncoder : GameState -> Json.Encode.Value
gameEncoder game =
    Json.Encode.object
        [ ( "association", GameJson.associationScenarioEncoder game.association )
        , ( "singleRel", GameJson.singleRelScenarioEncoder game.singleRel )
        , ( "twoRel", GameJson.twoRelScenarioEncoder game.twoRel )
        ]


gameDecoder : Json.Decode.Decoder GameState
gameDecoder =
    Json.Decode.map3
        GameState
        (Json.Decode.field "association" GameJson.associationScenarioDecoder)
        (Json.Decode.field "singleRel" GameJson.singleRelScenarioDecoder)
        (Json.Decode.field "twoRel" GameJson.twoRelScenarioDecoder)


gameHasData : GameState -> Bool
gameHasData game =
    let
        hasDataFunc =
            \sc ->
                case sc.history of
                    _ :: _ :: _ ->
                        True

                    singleInst :: [] ->
                        not (List.isEmpty singleInst.data)

                    [] ->
                        False
    in
    hasDataFunc game.association
        || hasDataFunc game.singleRel
        || hasDataFunc game.twoRel


main : Program Json.Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = always Sub.none
        }
