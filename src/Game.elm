module Game exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed
import Names
import Random
import Round
import Utils
import View


type ViewSettings
    = DotPlot
    | Contingency
    | Both


type alias Scenario spec experiment outcome guess =
    { history : List (HistoryItem spec experiment outcome guess)
    , proposedExperiment : experiment
    , proposedGuess : guess

    --    , activeChallenge : Maybe ChallengeState
    }



-- type HistoryItem spec experiment outcome guess
--     = AnInstance (Instance spec experiment outcome guess)
--     | ChallengeStarted
--     | ChallengeFailed ChallengeState
--     | ChallengeCancelled ChallengeState
--     | ChallengeSuccess ChallengeState


type alias HistoryItem spec experiment outcome guess =
    Instance spec experiment outcome guess


type alias Instance spec experiment outcome guess =
    { spec : spec
    , creatureName : String
    , data : List ( experiment, outcome )
    , guess : Maybe guess
    }



-- type alias ChallengeState =
--     { nInstances : Int
--     , nCorrect : Int
--     , totalCost : Int
--     }


type alias InitAdapter guess experiment =
    { defaultGuess : guess
    , defaultExperiment : experiment
    , instancesToAverage : Int
    }


type alias LogicAdapter expMsg guessMsg spec experiment outcome guess =
    { specGenerator : Random.Generator spec
    , generator : spec -> experiment -> Random.Generator outcome
    , updateExperiment : expMsg -> experiment -> experiment
    , updateGuess : guessMsg -> guess -> guess
    , guessEval : spec -> guess -> GuessEval
    , costExperiment : experiment -> Int
    }


type alias ViewAdapter expMsg guessMsg spec experiment outcome guess =
    { viewHeader : Html Never
    , viewInstanceGoal : spec -> Html Never
    , viewExperiment : ViewSettings -> spec -> Int -> ( experiment, outcome ) -> Html Never
    , viewProposedExperiment : spec -> experiment -> Html expMsg
    , viewCostCommentary : Html Never
    , viewGuess : spec -> guess -> Html Never
    , viewProposedGuess : spec -> guess -> Html guessMsg
    }


type alias Adapter expMsg guessMsg spec experiment outcome guess =
    { init : InitAdapter guess experiment
    , logic : LogicAdapter expMsg guessMsg spec experiment outcome guess
    , view : ViewAdapter expMsg guessMsg spec experiment outcome guess
    }


type Msg expMsg guessMsg spec experiment outcome guess
    = SpecGenerated ( String, spec )
    | RunExperiment
    | DataGenerated experiment outcome
    | ExperimentChanged expMsg
    | GuessChanged guessMsg
    | MakeGuess
    | NewInstance
    | SetViewSettings ViewSettings


type alias GuessEval =
    ( Float, Html Never )


init : Adapter expMsg guessMsg spec experiment outcome guess -> Scenario spec experiment outcome guess
init adapter =
    { history = []
    , proposedExperiment = adapter.init.defaultExperiment
    , proposedGuess = adapter.init.defaultGuess

    --    , activeChallenge = Nothing
    }


initCmd :
    Adapter expMsg guessMsg spec experiment outcome guess
    -> Cmd (Msg expMsg guessMsg spec experiment outcome guess)
initCmd adapter =
    Random.generate SpecGenerated (instanceGenerator adapter.logic.specGenerator)


type alias UpdateResult expMsg guessMsg spec experiment outcome guess =
    { scenario : Scenario spec experiment outcome guess
    , cmd : Cmd (Msg expMsg guessMsg spec experiment outcome guess)
    , viewSettings : ViewSettings
    , updateStorage : Bool
    }


update :
    ViewSettings
    -> Adapter expMsg guessMsg spec experiment outcome guess
    -> Msg expMsg guessMsg spec experiment outcome guess
    -> Scenario spec experiment outcome guess
    -> UpdateResult expMsg guessMsg spec experiment outcome guess
update viewSettings adapter msg scenario =
    case msg of
        SpecGenerated ( name, sp ) ->
            UpdateResult { scenario | history = { spec = sp, data = [], guess = Nothing, creatureName = name } :: scenario.history, proposedExperiment = adapter.init.defaultExperiment } Cmd.none viewSettings True

        RunExperiment ->
            case scenario.history of
                head :: _ ->
                    UpdateResult scenario (Random.generate (DataGenerated scenario.proposedExperiment) (adapter.logic.generator head.spec scenario.proposedExperiment)) viewSettings False

                _ ->
                    UpdateResult scenario Cmd.none viewSettings False

        DataGenerated exp data ->
            case scenario.history of
                active :: rest ->
                    if allowMoreExperiments active then
                        UpdateResult { scenario | history = { active | data = ( exp, data ) :: active.data } :: rest } Cmd.none viewSettings True

                    else
                        UpdateResult scenario Cmd.none viewSettings False

                _ ->
                    UpdateResult scenario Cmd.none viewSettings False

        ExperimentChanged eMsg ->
            UpdateResult { scenario | proposedExperiment = adapter.logic.updateExperiment eMsg scenario.proposedExperiment } Cmd.none viewSettings False

        GuessChanged gMsg ->
            UpdateResult { scenario | proposedGuess = adapter.logic.updateGuess gMsg scenario.proposedGuess } Cmd.none viewSettings False

        MakeGuess ->
            case scenario.history of
                active :: rest ->
                    UpdateResult
                        { scenario
                            | history = { active | guess = Just scenario.proposedGuess } :: rest
                            , proposedGuess = adapter.init.defaultGuess
                        }
                        Cmd.none
                        viewSettings
                        True

                -- TODO update challenge state
                _ ->
                    UpdateResult scenario Cmd.none viewSettings False

        NewInstance ->
            UpdateResult scenario (Random.generate SpecGenerated (instanceGenerator adapter.logic.specGenerator)) viewSettings False

        SetViewSettings newSettings ->
            UpdateResult scenario Cmd.none newSettings False


instanceGenerator : Random.Generator spec -> Random.Generator ( String, spec )
instanceGenerator specGenerator =
    Random.map2 Tuple.pair Names.creatureNameGenerator specGenerator


allowMoreExperiments : Instance spec experiment outcome guess -> Bool
allowMoreExperiments instance =
    List.length instance.data < 10


view :
    ViewSettings
    -> Adapter expMsg guessMsg spec experiment outcome guess
    -> Scenario spec experiment outcome guess
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
view viewSettings adapter scenario =
    div [ Attr.class "scenario" ]
        [ viewGameControls adapter scenario
        , viewHistory viewSettings adapter scenario.history
        , div [ Attr.class "scenarioFooter" ] []
        ]


getResults :
    LogicAdapter expMsg guessMsg spec experiment outcome guess
    -> List (HistoryItem spec experiment outcome guess)
    -> List ( Float, Int )
getResults adapter history =
    case history of
        head :: rest ->
            case head.guess of
                Just guess ->
                    let
                        eval =
                            Tuple.first (adapter.guessEval head.spec guess)
                    in
                    ( eval, computeCost adapter head )
                        :: getResults adapter rest

                Nothing ->
                    getResults adapter rest

        [] ->
            []


viewStats :
    Adapter expMsg guessMsg spec experiment outcome guess
    -> Scenario spec experiment outcome guess
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
viewStats adapter scenario =
    let
        ( correct, cost ) =
            getResults adapter.logic scenario.history
                |> List.unzip

        correctShort =
            List.take adapter.init.instancesToAverage correct

        costShort =
            List.take adapter.init.instancesToAverage cost

        nRes =
            List.length correct

        propCorrect =
            correct |> Utils.safeAverageF

        propCorrectShort =
            correctShort |> Utils.safeAverageF

        avgCost =
            Utils.safeAverage cost

        avgCostShort =
            Utils.safeAverage costShort
    in
    div [ Attr.class "stats" ]
        (h3 [] [ text "Results summary" ]
            :: (if nRes == 0 then
                    [ text "No instances completed yet." ]

                else
                    [ strong [] [ text "All ", text (String.fromInt nRes), text " instances: " ]
                    , text (Round.round 0 (propCorrect * 100) ++ "% correct, avg cost: CZK " ++ String.fromInt (round avgCost))
                    , br [] []
                    , strong [] [ text "Last ", text (String.fromInt adapter.init.instancesToAverage), text " instances: " ]
                    , if nRes >= adapter.init.instancesToAverage then
                        text (Round.round 0 (propCorrectShort * 100) ++ "% correct, avg cost: CZK " ++ String.fromInt (round avgCostShort))

                      else
                        text "--"
                    ]
               )
        )


viewGameControls :
    Adapter expMsg guessMsg spec experiment outcome guess
    -> Scenario spec experiment outcome guess
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
viewGameControls adapter scenario =
    case scenario.history of
        instance :: _ ->
            let
                ( wasGuessed, guessElement ) =
                    case instance.guess of
                        Just _ ->
                            ( True, text "" )

                        Nothing ->
                            ( False
                            , case instance.data of
                                _ :: _ ->
                                    div [ Attr.class "proposedGuess" ]
                                        [ h3 [] [ text "Ready to make a guess?" ]
                                        , Html.map GuessChanged (adapter.view.viewProposedGuess instance.spec scenario.proposedGuess)
                                        , button [ Attr.type_ "button", Attr.class "guessButton", Events.onClick MakeGuess ] [ text "Make a guess!" ]
                                        ]

                                [] ->
                                    text ""
                            )

                activeElement =
                    if not wasGuessed then
                        div []
                            [ h3 [] [ text "Run an experiment" ]
                            , Html.map ExperimentChanged (adapter.view.viewProposedExperiment instance.spec scenario.proposedExperiment)
                            , if allowMoreExperiments instance then
                                div []
                                    [ Html.map never adapter.view.viewCostCommentary
                                    , br [] []
                                    , button [ Attr.type_ "button", Events.onClick RunExperiment ] [ text ("Gather more data for CZK " ++ String.fromInt (adapter.logic.costExperiment scenario.proposedExperiment)) ]
                                    ]

                              else
                                text "Reached the maximum number of experiments"
                            , div []
                                [ strong [] [ text "Total cost so far: " ]
                                , text ("CZK " ++ String.fromInt (computeCost adapter.logic instance))
                                ]
                            ]

                    else
                        div []
                            [ strong [] [ text "Total cost: " ]
                            , text ("CZK " ++ String.fromInt (computeCost adapter.logic instance))
                            ]
            in
            div [ Attr.class "controls" ]
                [ Html.map never adapter.view.viewHeader
                , viewStats adapter scenario

                --, div [ Attr.class "afterHeader" ] []
                , div [ Attr.class "scenarioControl" ] [ viewScenarioControl adapter scenario ]
                , case scenario.history of
                    activeInstance :: _ ->
                        div []
                            [ h3 []
                                [ text ("Instance " ++ String.fromInt (List.length scenario.history) ++ ": ")
                                , em [] [ text activeInstance.creatureName ]
                                ]
                            , strong [] [ text "Goal: " ]
                            , Html.map never (adapter.view.viewInstanceGoal activeInstance.spec)
                            ]

                    _ ->
                        text ""
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
                instance :: _ ->
                    case instance.guess of
                        Just _ ->
                            True

                        Nothing ->
                            False

                [] ->
                    True
    in
    if allowNewInstance then
        div [] [ button [ Attr.type_ "button", Events.onClick NewInstance ] [ text "Start new instance" ] ]

    else
        text ""


computeCost : LogicAdapter expMsg guessMsg spec experiment outcome guess -> Instance spec experiment outcome guess -> Int
computeCost adapter instance =
    List.map Tuple.first instance.data
        |> List.map adapter.costExperiment
        |> List.sum



--todo activeChallenge


withReverseIds : List a -> List ( String, a )
withReverseIds items =
    let
        ids =
            List.range 0 (List.length items - 1)
                |> List.reverse
                |> List.map String.fromInt
    in
    List.map2 Tuple.pair ids items


reverseIndexedMap : (Int -> a -> b) -> List a -> List b
reverseIndexedMap f l =
    let
        ids =
            List.range 0 (List.length l - 1)
                |> List.reverse
    in
    List.map2 f ids l


viewHistory :
    ViewSettings
    -> Adapter expMsg guessMsg spec experiment outcome guess
    -> List (HistoryItem spec experiment outcome guess)
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
viewHistory viewSettings adapter historyList =
    case historyList of
        head :: rest ->
            let
                activeId =
                    List.length historyList - 1

                activeScenario =
                    viewSingleHistory viewSettings adapter True activeId head

                breakElement =
                    case rest of
                        _ :: _ ->
                            h2 [ Attr.class "historyStart" ] [ text "Previous instances " ]

                        [] ->
                            text ""

                history =
                    reverseIndexedMap (viewSingleHistory viewSettings adapter False) rest
            in
            Html.Keyed.node
                "div"
                []
                (( String.fromInt activeId, activeScenario )
                    :: ( "break", breakElement )
                    :: withReverseIds history
                )

        [] ->
            div [] [ text "No instances yet" ]


viewSingleHistory :
    ViewSettings
    -> Adapter expMsg guessMsg spec experiment outcome guess
    -> Bool
    -> Int
    -> HistoryItem spec experiment outcome guess
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
viewSingleHistory viewSettings adapter active id instance =
    let
        experiments =
            reverseIndexedMap (adapter.view.viewExperiment viewSettings instance.spec) instance.data
                |> List.map (Html.map never)

        dataDesc =
            case experiments of
                _ :: _ ->
                    h3 [] [ text "Your data:" ]

                [] ->
                    h3 [] [ text "No data to show." ]
    in
    div [ Attr.classList [ ( "instance", True ), ( "history", not active ) ] ]
        [ if active then
            text ""

          else
            h3 []
                [ text ("Instance " ++ String.fromInt (id + 1) ++ ": ")
                , em [] [ text instance.creatureName ]
                ]
        , if active then
            text ""

          else
            div []
                [ strong [] [ text "Total cost: " ]
                , text ("CZK " ++ String.fromInt (computeCost adapter.logic instance))
                ]
        , case instance.guess of
            Nothing ->
                text ""

            Just guess ->
                let
                    ( correct, desc ) =
                        adapter.logic.guessEval instance.spec guess

                    guessResultDescription =
                        div []
                            [ text "The guess was "
                            , strong []
                                [ text
                                    (if correct >= 1.0 then
                                        "CORRECT"

                                     else if correct <= 0.0 then
                                        "INCORRECT"

                                     else
                                        String.fromInt (round (correct * 100)) ++ "% CORRECT"
                                    )
                                ]
                            , div [ Attr.class "guessResultDesc" ] [ Html.map never desc ]
                            ]
                in
                div [ Attr.class "guessArea" ]
                    [ h4 [] [ text "Your guess: " ]
                    , div [ Attr.class "guess" ] [ Html.map never (adapter.view.viewGuess instance.spec guess) ]
                    , guessResultDescription
                    ]
        , viewViewSettings viewSettings
        , dataDesc
        , Html.Keyed.node "div" [ Attr.class "experiments" ] (withReverseIds experiments)
        ]


viewViewSettings : ViewSettings -> Html (Msg expMsg guessMsg spec experiment outcome guess)
viewViewSettings viewSettings =
    let
        singleOption =
            \x ->
                option [ Attr.selected (viewSettings == x), Attr.value (viewSettingsToString x) ] [ text (viewSettingsToString x) ]
    in
    div []
        [ text "View experiment results as "
        , select [ View.onChange (stringToViewSettings >> SetViewSettings) ]
            [ singleOption DotPlot
            , singleOption Contingency
            , singleOption Both
            ]
        ]


viewSettingsToString : ViewSettings -> String
viewSettingsToString v =
    case v of
        DotPlot ->
            "Dots"

        Contingency ->
            "Contingency"

        Both ->
            "Both"


stringToViewSettings : String -> ViewSettings
stringToViewSettings s =
    case s of
        "Dots" ->
            DotPlot

        "Contingency" ->
            Contingency

        "Both" ->
            Both

        _ ->
            DotPlot
