module Main exposing (..)

import Association
import Browser
import Game
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import TwoWayCausality


type Page
    = AssocPage
    | TwoWayPage


type alias Model =
    { page : Page
    , association : Association.Model
    , twoWay : TwoWayCausality.Model
    }


type Msg
    = AssocMsg Association.Msg
    | TwoWayMsg TwoWayCausality.Msg
    | ActivatePage Page


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = AssocPage
      , association = Game.init Association.adapter
      , twoWay = Game.init TwoWayCausality.adapter
      }
    , Cmd.batch
        [ Cmd.map AssocMsg (Game.initCmd Association.adapter)
        , Cmd.map TwoWayMsg (Game.initCmd TwoWayCausality.adapter)
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

        TwoWayMsg twoWayMsg ->
            let
                ( newTwoWayModel, twoWayCmd ) =
                    Game.update TwoWayCausality.adapter twoWayMsg model.twoWay
            in
            ( { model | twoWay = newTwoWayModel }, Cmd.map TwoWayMsg twoWayCmd )

        ActivatePage page ->
            ( { model | page = page }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ Attr.style "margin" "1em" ]
        [ viewPageSelection model
        , hr [ Attr.style "clear" "both" ] []
        , div [ Attr.style "display" (ifActive ( model.page, AssocPage ) ( "block", "none" )) ]
            [ Html.map AssocMsg (Game.view Association.adapter model.association)
            ]
        , div [ Attr.style "display" (ifActive ( model.page, TwoWayPage ) ( "block", "none" )) ]
            [ Html.map TwoWayMsg (Game.view TwoWayCausality.adapter model.twoWay)
            ]
        ]


viewPageSelection : Model -> Html Msg
viewPageSelection model =
    div [] (List.map (viewPageSelectionButton model.page) [ AssocPage, TwoWayPage ])


viewPageSelectionButton : Page -> Page -> Html Msg
viewPageSelectionButton activePage page =
    let
        baseStyle =
            [ Attr.style "float" "left"
            , Attr.style "font-decoration" "underline"
            , Attr.style "border-top" "1px solid"
            , Attr.style "border-left" "1px solid"
            , Attr.style "border-right" "1px solid"
            , Attr.style "margin" "0.5em"
            , Attr.style "padding" "0.5em"
            , Attr.style "display" "block"
            , Attr.style "cursor" "pointer"
            ]
    in
    ifActive ( activePage, page )
        ( h2 (Attr.style "border-color" "black" :: baseStyle) [ text (pageTitle page) ]
        , h2 (Attr.style "border-color" "gray" :: Attr.style "color" "gray" :: baseStyle) [ a [ Events.onClick (ActivatePage page) ] [ text (pageTitle page) ] ]
        )


pageTitle : Page -> String
pageTitle page =
    case page of
        AssocPage ->
            "Association"

        TwoWayPage ->
            "Two way causality"


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
