module Main exposing (..)

import Association
import Browser
import Game
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import SingleRelationship
import TwoRelationships
import UnobservedConfounding


type Page
    = AssocPage
    | SingleRelPage
    | TwoRelPage
    | UnobsPage


type alias Model =
    { page : Page
    , association : Association.Model
    , singleRel : SingleRelationship.Model
    , twoRel : TwoRelationships.Model
    , unobs : UnobservedConfounding.Model
    }


type Msg
    = AssocMsg Association.Msg
    | SingleRel SingleRelationship.Msg
    | TwoRel TwoRelationships.Msg
    | Unobs UnobservedConfounding.Msg
    | ActivatePage Page


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = AssocPage
      , association = Game.init Association.adapter
      , singleRel = Game.init SingleRelationship.adapter
      , twoRel = Game.init TwoRelationships.adapter
      , unobs = Game.init UnobservedConfounding.adapter
      }
    , Cmd.batch
        [ Cmd.map AssocMsg (Game.initCmd Association.adapter)
        , Cmd.map SingleRel (Game.initCmd SingleRelationship.adapter)
        , Cmd.map TwoRel (Game.initCmd TwoRelationships.adapter)
        , Cmd.map Unobs (Game.initCmd UnobservedConfounding.adapter)
        ]
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

        SingleRel singleMsg ->
            let
                ( newSingleRelModel, singleCmd ) =
                    Game.update SingleRelationship.adapter singleMsg model.singleRel
            in
            ( { model | singleRel = newSingleRelModel }, Cmd.map SingleRel singleCmd )

        TwoRel twoWayMsg ->
            let
                ( newTwoWayModel, twoWayCmd ) =
                    Game.update TwoRelationships.adapter twoWayMsg model.twoRel
            in
            ( { model | twoRel = newTwoWayModel }, Cmd.map TwoRel twoWayCmd )

        Unobs unobsMsg ->
            let
                ( newModel, newCmd ) =
                    Game.update UnobservedConfounding.adapter unobsMsg model.unobs
            in
            ( { model | unobs = newModel }, Cmd.map Unobs newCmd )

        ActivatePage page ->
            ( { model | page = page }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ Attr.class "topContainer" ]
        [ viewPageSelection model
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, AssocPage ) ( "block", "none" )) ]
            [ Html.map AssocMsg (Game.view Association.adapter model.association)
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, SingleRelPage ) ( "block", "none" )) ]
            [ Html.map SingleRel (Game.view SingleRelationship.adapter model.singleRel)
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, TwoRelPage ) ( "block", "none" )) ]
            [ Html.map TwoRel (Game.view TwoRelationships.adapter model.twoRel)
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, UnobsPage ) ( "block", "none" )) ]
            [ Html.map Unobs (Game.view UnobservedConfounding.adapter model.unobs)
            ]
        ]


viewPageSelection : Model -> Html Msg
viewPageSelection model =
    div [] (List.map (viewPageSelectionButton model.page) [ AssocPage, SingleRelPage, TwoRelPage, UnobsPage ])


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

        UnobsPage ->
            "3: Unobserved confounding"


ifActive : ( Page, Page ) -> ( a, a ) -> a
ifActive ( activePage, page ) ( activeOutput, inactiveOutput ) =
    if page == activePage then
        activeOutput

    else
        inactiveOutput


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
