port module Main exposing (..)

import Association
import Base64
import Browser
import Causality
import Game exposing (getResults)
import GameState
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import Json.Encode
import Round
import SingleRelationship
import ThreeWay
import TwoRelationships
import UnobservedConfounding
import Utils


type Page
    = AssocPage
    | SingleRelPage
    | TwoRelPage
    | ThreeWayPage


type alias Model =
    { page : Page
    , game : GameState.GameState
    , storedGame : Maybe GameState.GameState
    , homeworkSubmission : Bool
    , homeworkName : String
    , homeworkGroup : String
    , homeworkInfoShow : Bool
    }


type Msg
    = AssocMsg Association.Msg
    | SingleRel SingleRelationship.Msg
    | TwoRel TwoRelationships.Msg
    | ThreeWay ThreeWay.Msg
    | ActivatePage Page
    | LoadStored
    | DismissStored
    | ShowHomeworkInfo Bool
    | HomeworkSubmissionStart
    | HomeworkSubmissionEnd
    | HomeworkSetName String
    | HomeworkSetGroup String


port setStorage : Json.Encode.Value -> Cmd msg


init : Json.Encode.Value -> ( Model, Cmd Msg )
init storedGame =
    ( { page = AssocPage
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

                Err a ->
                    Tuple.first ( Nothing, Debug.log "Err JSON: " a )
      , homeworkSubmission = False
      , homeworkName = ""
      , homeworkGroup = ""
      , homeworkInfoShow = True
      }
    , Cmd.batch
        [ Cmd.map AssocMsg (Game.initCmd Association.adapter)
        , Cmd.map SingleRel (Game.initCmd SingleRelationship.adapter)
        , Cmd.map TwoRel (Game.initCmd TwoRelationships.adapter)
        , Cmd.map ThreeWay (Game.initCmd ThreeWay.adapter)
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

        ThreeWay subMsg ->
            let
                ( newTwoWayModel, newCmd ) =
                    Game.update ThreeWay.adapter subMsg model.game.threeWay
            in
            ( { model | game = { game | threeWay = newTwoWayModel } }, Cmd.map ThreeWay newCmd )

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

        HomeworkSubmissionStart ->
            ( { model | homeworkSubmission = True }, Cmd.none )

        HomeworkSubmissionEnd ->
            ( { model | homeworkSubmission = False }, Cmd.none )

        HomeworkSetName name ->
            ( { model | homeworkName = name }, Cmd.none )

        HomeworkSetGroup group ->
            ( { model | homeworkGroup = group }, Cmd.none )

        ShowHomeworkInfo show ->
            ( { model | homeworkInfoShow = show }, Cmd.none )


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
            if GameState.gameHasData newModel.game then
                Cmd.batch [ setStorage (GameState.gameEncoder newModel.game), cmds ]

            else
                cmds
    )


view : Model -> Html Msg
view model =
    div [ Attr.class "topContainer" ]
        [ viewHomeworkControls model
        , viewPageSelection model
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, AssocPage ) ( "block", "none" )) ]
            [ Html.map AssocMsg (Game.view Association.adapter model.game.association)
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, SingleRelPage ) ( "block", "none" )) ]
            [ Html.map SingleRel (Game.view SingleRelationship.adapter model.game.singleRel)
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, TwoRelPage ) ( "block", "none" )) ]
            [ Html.map TwoRel (Game.view TwoRelationships.adapter model.game.twoRel)
            ]
        , div [ Attr.class "scenarioPage", Attr.style "display" (ifActive ( model.page, ThreeWayPage ) ( "block", "none" )) ]
            [ Html.map ThreeWay (Game.view ThreeWay.adapter model.game.threeWay)
            ]
        , case model.storedGame of
            Just _ ->
                viewStoredPopup

            Nothing ->
                if model.homeworkSubmission then
                    viewHomeworkPopup model

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


viewHomeworkPopup : Model -> Html Msg
viewHomeworkPopup model =
    let
        encodedHomework =
            if not (String.isEmpty model.homeworkName) && not (String.isEmpty model.homeworkGroup) then
                Just
                    (Json.Encode.object
                        [ ( "name", Json.Encode.string model.homeworkName )
                        , ( "group", Json.Encode.string model.homeworkGroup )
                        , ( "state", GameState.gameEncoder model.game )
                        ]
                        |> Json.Encode.encode 0
                        |> String.toList
                        |> List.map Char.toCode
                        |> Base64.encode
                        |> Result.withDefault "RXJyb3IgNTYyNw=="
                    )
                --todo data url here

            else
                Nothing

        score =
            .score (computeHomeworkScore (getResults TwoRelationships.adapter.logic model.game.twoRel.history))
    in
    div [ Attr.class "popupBackground" ]
        [ div [ Attr.class "popup" ]
            [ p []
                [ text "To submit your homework, fill in your name and group. Then use the link below to download a file and send it to martin.modrak@lfmotol.cuni.cz" ]
            , p []
                [ text "Your homework is currently worth ", strong [] [ text (String.fromInt score) ], text " points." ]
            , text "Your full name: "
            , input [ Attr.type_ "text", Events.onInput HomeworkSetName, Attr.value model.homeworkName ] []
            , br [] []
            , text "Your group: "
            , input [ Attr.type_ "text", Events.onInput HomeworkSetGroup, Attr.value model.homeworkGroup ] []
            , br [] []
            , p [ Attr.class "downloadHW", Attr.class "left" ]
                [ case encodedHomework of
                    Just enc ->
                        a [ Attr.href ("data:application/json;base64," ++ enc), Attr.download "homework.json" ] [ strong [] [ text "Download homework file" ] ]

                    Nothing ->
                        strong [] [ text "Enter name and group to enable download" ]
                ]
            , button [ Attr.type_ "button", Attr.class "right", Events.onClick HomeworkSubmissionEnd ] [ text "Back to game" ]
            ]
        ]


viewPageSelection : Model -> Html Msg
viewPageSelection model =
    div [] (List.map (viewPageSelectionButton model.page) [ AssocPage, SingleRelPage, TwoRelPage, ThreeWayPage ])


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


viewHomeworkControls : Model -> Html Msg
viewHomeworkControls model =
    let
        eval =
            computeHomeworkScore (getResults TwoRelationships.adapter.logic model.game.twoRel.history)
    in
    div [ Attr.class "homework" ]
        [ h3 [] [ text "Homework scoring" ]
        , p []
            [ text "Currently, you would get "
            , strong [] [ text (String.fromInt eval.score) ]
            , text " points."
            , text " "
            ]
        , p []
            [ if eval.nInstances >= 3 then
                text ""

              else
                div [] [ text "You need to complete at least 3 instances of the homework problem to gain points." ]
            , text "Your average correctness and average cost are computed over 3 consecutive instances of the homework problem."
            , div []
                [ text
                    (case eval.score of
                        4 ->
                            "This is the maximum and is achieved when you obtain >" ++ Round.round 0 (100 * avgCorrectForThreePoints) ++ "% correct with average cost < CZK " ++ Round.round 0 avgCostForFourPoints ++ " over three consecutive instances of the homework problem."

                        3 ->
                            "This is achieved when you obtain >" ++ Round.round 0 (100 * avgCorrectForThreePoints) ++ "% correct with average cost < CZK " ++ Round.round 0 avgCostForThreePoints ++ " over three consecutive instances of the homework problem."

                        2 ->
                            "This is achieved when you obtain >" ++ Round.round 0 (100 * avgCorrectForTwoPoints) ++ "% correct regardless of cost over three consecutive instances of the homework problem."

                        1 ->
                            "This is achieved when you obtain >" ++ Round.round 0 (100 * avgCorrectForOnePoint) ++ "% correct regardless of cost over three consecutive instances of the homework problem."

                        _ ->
                            ""
                    )
                ]
            , if eval.nInstances >= 3 then
                text
                    ("Your best score was obtained from instances "
                        ++ String.fromInt eval.startingAt
                        ++ " - "
                        ++ String.fromInt (eval.startingAt + 2)
                        ++ " where you had "
                        ++ Round.round 0 (100 * eval.avgCorrect)
                        ++ "% correct with average cost of CZK "
                        ++ Round.round 0 eval.avgCost
                    )

              else
                text ""
            , ul []
                [ if eval.score < 1 then
                    li [] [ text ("To gain 1 point, you need >" ++ Round.round 0 (100 * avgCorrectForOnePoint) ++ "% correct regardless of cost.") ]

                  else
                    text ""
                , if eval.score < 2 then
                    li [] [ text ("To gain 2 points you need, >" ++ Round.round 0 (100 * avgCorrectForTwoPoints) ++ "% correct regardless of cost.") ]

                  else
                    text ""
                , if eval.score < 3 then
                    li [] [ text ("To gain 3 points, you need >" ++ Round.round 0 (100 * avgCorrectForThreePoints) ++ "% correct with average cost < CZK " ++ Round.round 0 avgCostForThreePoints ++ ".") ]

                  else
                    text ""
                , if eval.score < 4 then
                    li [] [ text ("To gain 4 points, you need >" ++ Round.round 0 (100 * avgCorrectForThreePoints) ++ "% correct with average cost < CZK " ++ Round.round 0 avgCostForFourPoints ++ ".") ]

                  else
                    text ""
                ]
            , text "You cannot loose points by trying some more. Your progress is stored in your browser (you can close this window and return later)."
            ]
        , button [ Attr.type_ "button", Events.onClick HomeworkSubmissionStart ] [ strong [] [ text "Download homework result" ] ]
        ]


type alias HomeworkEval =
    { score : Int
    , startingAt : Int
    , avgCorrect : Float
    , avgCost : Float
    , nInstances : Int
    }


avgCorrectForOnePoint : Float
avgCorrectForOnePoint =
    0.5


avgCorrectForTwoPoints : Float
avgCorrectForTwoPoints =
    0.8


avgCorrectForThreePoints : Float
avgCorrectForThreePoints =
    0.85


avgCostForThreePoints : Float
avgCostForThreePoints =
    500000


avgCostForFourPoints : Float
avgCostForFourPoints =
    300000


computeHomeworkScore : List ( Float, Int ) -> HomeworkEval
computeHomeworkScore results =
    case results of
        r0 :: r1 :: r2 :: rest ->
            let
                recursion =
                    computeHomeworkScore (r1 :: r2 :: rest)

                currentCost =
                    Utils.safeAverage (List.map Tuple.second [ r0, r1, r2 ])

                currentCorrect =
                    Utils.safeAverageF (List.map Tuple.first [ r0, r1, r2 ])

                currentScore =
                    if currentCorrect < avgCorrectForOnePoint then
                        0

                    else if currentCorrect < avgCorrectForTwoPoints then
                        1

                    else if currentCorrect < avgCorrectForThreePoints || currentCost > avgCostForThreePoints then
                        2

                    else if currentCost > avgCostForFourPoints then
                        3

                    else
                        4

                candidateRes =
                    { recursion
                        | score = currentScore
                        , avgCorrect = currentCorrect
                        , avgCost = currentCost
                        , startingAt = recursion.nInstances
                        , nInstances = recursion.nInstances + 1
                    }

                defaultRes =
                    { recursion | nInstances = recursion.nInstances + 1 }
            in
            if currentScore > recursion.score then
                candidateRes

            else if currentScore == recursion.score then
                if currentCorrect >= avgCorrectForThreePoints && currentCost < recursion.avgCost then
                    candidateRes

                else if currentCorrect > recursion.avgCorrect then
                    candidateRes

                else
                    defaultRes

            else
                defaultRes

        _ :: rest ->
            let
                recursion =
                    computeHomeworkScore rest
            in
            { recursion | nInstances = recursion.nInstances + 1 }

        [] ->
            { score = 0
            , startingAt = 0
            , avgCorrect = 0
            , avgCost = 0
            , nInstances = 0
            }


main : Program Json.Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = always Sub.none
        }
