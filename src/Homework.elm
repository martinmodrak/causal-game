module Homework exposing (..)

import Base64
import Base64String
import Crypto.Strings
import Game
import GameState
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import Json.Encode
import Random
import Round
import TwoRelationships
import Utils


type alias PreparedSubmission =
    { url : String
    , confirmationCode : String
    }


type alias Model =
    { submission : Bool
    , name : String
    , group : String
    , preparedSubmission : Maybe PreparedSubmission
    }


type Msg
    = SubmissionStart
    | SubmissionEnd
    | SetName String
    | SetGroup String
    | PrepareDownload


init : Model
init =
    { submission = False
    , name = ""
    , group = ""
    , preparedSubmission = Nothing
    }


update : Msg -> GameState.GameState -> Model -> ( Model, Cmd Msg )
update msg game model =
    case msg of
        SubmissionStart ->
            ( { model
                | submission = True
                , preparedSubmission =
                    if allowDownload model then
                        Just (buildPreparedSubmission game model)

                    else
                        Nothing
              }
            , Cmd.none
            )

        SubmissionEnd ->
            ( { model | submission = False }, Cmd.none )

        SetName name ->
            ( { model | name = name, preparedSubmission = Nothing }, Cmd.none )

        SetGroup group ->
            ( { model | group = group, preparedSubmission = Nothing }, Cmd.none )

        PrepareDownload ->
            ( { model
                | preparedSubmission =
                    if allowDownload model then
                        Just (buildPreparedSubmission game model)

                    else
                        Nothing
              }
            , Cmd.none
            )


viewControls : GameState.GameState -> Html Msg
viewControls game =
    let
        eval =
            computeScore game
    in
    div [ Attr.class "" ]
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
                            "This is the maximum and is achieved when you obtain >" ++ Round.round 0 (100 * avgCorrectForThreePoints) ++ "% correct with average cost < CZK " ++ Round.round 0 avgCostForFourPoints ++ " over three consecutive instances of the  problem."

                        3 ->
                            "This is achieved when you obtain >" ++ Round.round 0 (100 * avgCorrectForThreePoints) ++ "% correct with average cost < CZK " ++ Round.round 0 avgCostForThreePoints ++ " over three consecutive instances of the  problem."

                        2 ->
                            "This is achieved when you obtain >" ++ Round.round 0 (100 * avgCorrectForTwoPoints) ++ "% correct regardless of cost over three consecutive instances of the  problem."

                        1 ->
                            "This is achieved when you obtain >" ++ Round.round 0 (100 * avgCorrectForOnePoint) ++ "% correct regardless of cost over three consecutive instances of the  problem."

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
        , button [ Attr.type_ "button", Events.onClick SubmissionStart ] [ strong [] [ text "Download  result" ] ]
        ]


type alias Eval =
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
    0.8


avgCostForThreePoints : Float
avgCostForThreePoints =
    500000


avgCostForFourPoints : Float
avgCostForFourPoints =
    300000


computeScoreFromResults : List ( Float, Int ) -> Eval
computeScoreFromResults results =
    case results of
        r0 :: r1 :: r2 :: rest ->
            let
                recursion =
                    computeScoreFromResults (r1 :: r2 :: rest)

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
                    computeScoreFromResults rest
            in
            { recursion | nInstances = recursion.nInstances + 1 }

        [] ->
            { score = 0
            , startingAt = 0
            , avgCorrect = 0
            , avgCost = 0
            , nInstances = 0
            }


computeScore : GameState.GameState -> Eval
computeScore game =
    computeScoreFromResults (Game.getResults TwoRelationships.adapter.logic game.twoRel.history)


allowDownload : Model -> Bool
allowDownload model =
    not (String.isEmpty model.name) && not (String.isEmpty model.group)


getConfirmationCode : String -> String -> Int -> String
getConfirmationCode name group score =
    let
        encodedRaw =
            Crypto.Strings.encrypt (Random.initialSeed GameState.rs) GameState.ps (name ++ ":" ++ group ++ ":" ++ String.fromInt score)

        encoded =
            case encodedRaw of
                Ok ( enc, _ ) ->
                    enc

                Err _ ->
                    "68xadsf655aqq/=" ++ String.fromInt score ++ "fg56/="
    in
    name ++ ":" ++ group ++ ":" ++ encoded


buildPreparedSubmission : GameState.GameState -> Model -> PreparedSubmission
buildPreparedSubmission game model =
    { url =
        "data:application/json;base64,"
            ++ (Json.Encode.object
                    [ ( "base64name", Json.Encode.string (Base64String.encode model.name) )
                    , ( "base64group", Json.Encode.string (Base64String.encode model.group) )
                    , ( "state", GameState.gameEncoder game )
                    ]
                    |> Json.Encode.encode 0
                    |> Base64String.encode
               )
    , confirmationCode = getConfirmationCode model.name model.group (.score (computeScore game))
    }


viewPopup : GameState.GameState -> Model -> Html Msg
viewPopup game model =
    let
        score =
            .score (computeScore game)
    in
    div [ Attr.class "popupBackground" ]
        [ div [ Attr.class "popup" ]
            [ p []
                [ text "To submit your homework, fill in your name and group. Then use the link below to download a file verifying your results. Store the file on your device and send it as attachment to martin.modrak@lfmotol.cuni.cz" ]
            , p []
                [ text "Your homework is currently worth ", strong [] [ text (String.fromInt score) ], text " points." ]
            , text "Your full name: "
            , input [ Attr.type_ "text", Events.onInput SetName, Attr.value model.name ] []
            , br [] []
            , text "Your group: "
            , input [ Attr.type_ "text", Events.onInput SetGroup, Attr.value model.group ] []
            , br [] []
            , p [ Attr.class "downloadHW", Attr.class "left" ]
                [ case model.preparedSubmission of
                    Just submission ->
                        a [ Attr.href submission.url, Attr.download ("homework " ++ model.name ++ ".json") ] [ strong [] [ text "Download homework file" ] ]

                    Nothing ->
                        if allowDownload model then
                            button [ Attr.type_ "input", Events.onClick PrepareDownload ] [ text "Prepare download" ]

                        else
                            strong [] [ text "Enter name and group to enable download" ]
                ]
            , p [ Attr.class "hwCode", Attr.class "left" ]
                (case model.preparedSubmission of
                    Just submission ->
                        [ strong [] [ text "Confirmation code: " ]
                        , em [] [ text submission.confirmationCode ]
                        , br [] []
                        , text "Please store/screenshot this code as a backup."
                        ]

                    Nothing ->
                        []
                )
            , button [ Attr.type_ "button", Attr.class "right", Events.onClick SubmissionEnd ] [ text "Back to game" ]
            ]
        ]
