module Game exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Random


type alias Scenario spec experiment outcome guess =
    { history : List (HistoryItem spec experiment outcome guess)
    , proposedExperiment : experiment
    , proposedGuess : guess
    , activeChallenge : Maybe ChallengeState
    }


type HistoryItem spec experiment outcome guess
    = AnInstance (Instance spec experiment outcome guess)
    | ChallengeStarted
    | ChallengeFailed ChallengeState
    | ChallengeCancelled ChallengeState
    | ChallengeSuccess ChallengeState


type alias Instance spec experiment outcome guess =
    { spec : spec
    , data : List ( experiment, outcome )
    , guess : Maybe guess
    }


type alias ChallengeState =
    { nInstances : Int
    , nCorrect : Int
    , totalCost : Int
    }


type alias Adapter expMsg guessMsg spec experiment outcome guess =
    { defaultGuess : guess
    , defaultExperiment : experiment
    , specGenerator : Random.Generator spec
    , generator : spec -> experiment -> Random.Generator outcome
    , updateExperiment : expMsg -> experiment -> experiment
    , updateGuess : guessMsg -> guess -> guess
    , guessCorrect : spec -> guess -> Bool
    , costExperiment : experiment -> Int
    , viewExperiment : ( experiment, outcome ) -> Html Never
    , viewProposedExperiment : experiment -> Html expMsg
    , viewCostCommentary : Html Never
    , viewGuess : guess -> Html Never
    , viewProposedGuess : guess -> Html guessMsg
    , scenarioName : String
    }


type Msg expMsg guessMsg spec experiment outcome guess
    = SpecGenerated spec
    | RunExperiment
    | DataGenerated experiment outcome
    | ExperimentChanged expMsg
    | GuessChanged guessMsg
    | MakeGuess
    | NewInstance


init : Adapter expMsg guessMsg spec experiment outcome guess -> Scenario spec experiment outcome guess
init adapter =
    { history = []
    , proposedExperiment = adapter.defaultExperiment
    , proposedGuess = adapter.defaultGuess
    , activeChallenge = Nothing
    }


initCmd :
    Adapter expMsg guessMsg spec experiment outcome guess
    -> Cmd (Msg expMsg guessMsg spec experiment outcome guess)
initCmd adapter =
    Random.generate SpecGenerated adapter.specGenerator


update :
    Adapter expMsg guessMsg spec experiment outcome guess
    -> Msg expMsg guessMsg spec experiment outcome guess
    -> Scenario spec experiment outcome guess
    -> ( Scenario spec experiment outcome guess, Cmd (Msg expMsg guessMsg spec experiment outcome guess) )
update adapter msg scenario =
    case msg of
        SpecGenerated sp ->
            ( { scenario | history = AnInstance { spec = Debug.log "Spec: " sp, data = [], guess = Nothing } :: scenario.history }, Cmd.none )

        RunExperiment ->
            case scenario.history of
                (AnInstance head) :: _ ->
                    ( scenario, Random.generate (DataGenerated scenario.proposedExperiment) (adapter.generator head.spec scenario.proposedExperiment) )

                _ ->
                    ( scenario, Cmd.none )

        DataGenerated exp data ->
            case scenario.history of
                (AnInstance active) :: rest ->
                    if allowMoreExperiments active then
                        ( { scenario | history = AnInstance { active | data = ( exp, data ) :: active.data } :: rest }, Cmd.none )

                    else
                        ( scenario, Cmd.none )

                _ ->
                    ( scenario, Cmd.none )

        ExperimentChanged eMsg ->
            ( { scenario | proposedExperiment = adapter.updateExperiment eMsg scenario.proposedExperiment }, Cmd.none )

        GuessChanged gMsg ->
            ( { scenario | proposedGuess = adapter.updateGuess gMsg scenario.proposedGuess }, Cmd.none )

        MakeGuess ->
            case scenario.history of
                (AnInstance active) :: rest ->
                    ( { scenario
                        | history = AnInstance { active | guess = Just scenario.proposedGuess } :: rest
                        , proposedGuess = adapter.defaultGuess
                      }
                    , Cmd.none
                    )

                -- TODO update challenge state
                _ ->
                    ( scenario, Cmd.none )

        NewInstance ->
            ( scenario, Random.generate SpecGenerated adapter.specGenerator )


allowMoreExperiments : Instance spec experiment outcome guess -> Bool
allowMoreExperiments instance =
    List.length instance.data < 100


view :
    Adapter expMsg guessMsg spec experiment outcome guess
    -> Scenario spec experiment outcome guess
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
view adapter scenario =
    div [ Attr.class "scenario" ]
        [ viewGameControls adapter scenario
        , viewHistory adapter scenario.history
        ]


viewGameControls :
    Adapter expMsg guessMsg spec experiment outcome guess
    -> Scenario spec experiment outcome guess
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
viewGameControls adapter scenario =
    case scenario.history of
        (AnInstance instance) :: _ ->
            let
                ( wasGuessed, guessElement ) =
                    case instance.guess of
                        Just _ ->
                            ( True, text "" )

                        Nothing ->
                            ( False
                            , div [ Attr.class "proposedGuess" ]
                                [ h3 [] [ text "Ready to make a guess?" ]
                                , Html.map GuessChanged (adapter.viewProposedGuess scenario.proposedGuess)
                                , input [ Attr.type_ "button", Attr.class "guessButton", Events.onClick MakeGuess, Attr.value "Make a guess!" ] []
                                ]
                            )

                activeElement =
                    if not wasGuessed then
                        div []
                            [ Html.map ExperimentChanged (adapter.viewProposedExperiment scenario.proposedExperiment)
                            , if allowMoreExperiments instance then
                                div []
                                    [ Html.map never adapter.viewCostCommentary
                                    , br [] []
                                    , input [ Attr.type_ "button", Events.onClick RunExperiment, Attr.value ("Gather more data for CZK " ++ String.fromInt (adapter.costExperiment scenario.proposedExperiment)) ] []
                                    ]

                              else
                                text "Reached the maximum number of experiments"
                            , div []
                                [ strong [] [ text "Total cost so far: " ]
                                , text ("CZK " ++ String.fromInt (computeCost adapter instance))
                                ]
                            ]

                    else
                        div []
                            [ strong [] [ text "Total cost: " ]
                            , text ("CZK " ++ String.fromInt (computeCost adapter instance))
                            ]
            in
            div [ Attr.class "controls" ]
                [ h2 [] [ text adapter.scenarioName ]
                , viewScenarioControl adapter scenario
                , activeElement
                , guessElement
                ]

        _ ->
            text ""


viewScenarioControl :
    Adapter expMsg guessMsg spec experiment outcome guess
    -> Scenario spec experiment outcome guess
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
viewScenarioControl adapter scenario =
    let
        allowNewInstance =
            case scenario.history of
                (AnInstance instance) :: _ ->
                    case instance.guess of
                        Just _ ->
                            True

                        Nothing ->
                            False

                _ ->
                    True
    in
    if allowNewInstance then
        div [] [ input [ Attr.type_ "button", Events.onClick NewInstance, Attr.value "Start new instance" ] [] ]

    else
        text ""


computeCost : Adapter expMsg guessMsg spec experiment outcome guess -> Instance spec experiment outcome guess -> Int
computeCost adapter instance =
    List.map Tuple.first instance.data
        |> List.map adapter.costExperiment
        |> List.sum



--todo activeChallenge


viewHistory :
    Adapter expMsg guessMsg spec experiment outcome guess
    -> List (HistoryItem spec experiment outcome guess)
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
viewHistory adapter historyList =
    case historyList of
        head :: rest ->
            div []
                [ div [ Attr.class "active" ]
                    [ viewSingleHistory adapter True head
                    ]
                , div [ Attr.class "history" ]
                    (List.map (viewSingleHistory adapter False) rest)
                ]

        [] ->
            div [] [ text "No instances yet" ]


viewSingleHistory :
    Adapter expMsg guessMsg spec experiment outcome guess
    -> Bool
    -> HistoryItem spec experiment outcome guess
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
viewSingleHistory adapter active item =
    case item of
        AnInstance instance ->
            div [ Attr.class "instance" ]
                ((case instance.guess of
                    Nothing ->
                        text ""

                    Just guess ->
                        let
                            guessResultDescription =
                                if adapter.guessCorrect instance.spec guess then
                                    "Correct"

                                else
                                    "Incorrect"
                        in
                        div [ Attr.class "guess" ]
                            [ Html.map never (adapter.viewGuess guess)
                            , text "The guess was "
                            , strong [] [ text guessResultDescription ]
                            ]
                 )
                    :: List.map (adapter.viewExperiment >> Html.map never) instance.data
                )

        _ ->
            text "Not implemented yet"
