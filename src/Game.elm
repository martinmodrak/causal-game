module Game exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed
import Names
import Random
import Round
import Utils


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
    , viewExperiment : spec -> Int -> ( experiment, outcome ) -> Html Never
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


type alias GuessEval =
    ( Bool, Html Never )


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


update :
    Adapter expMsg guessMsg spec experiment outcome guess
    -> Msg expMsg guessMsg spec experiment outcome guess
    -> Scenario spec experiment outcome guess
    -> ( Scenario spec experiment outcome guess, Cmd (Msg expMsg guessMsg spec experiment outcome guess) )
update adapter msg scenario =
    case msg of
        SpecGenerated ( name, sp ) ->
            ( { scenario | history = { spec = Debug.log "spec:" sp, data = [], guess = Nothing, creatureName = name } :: scenario.history }, Cmd.none )

        RunExperiment ->
            case scenario.history of
                head :: _ ->
                    ( scenario, Random.generate (DataGenerated scenario.proposedExperiment) (adapter.logic.generator head.spec scenario.proposedExperiment) )

                _ ->
                    ( scenario, Cmd.none )

        DataGenerated exp data ->
            case scenario.history of
                active :: rest ->
                    if allowMoreExperiments active then
                        ( { scenario | history = { active | data = ( exp, data ) :: active.data } :: rest }, Cmd.none )

                    else
                        ( scenario, Cmd.none )

                _ ->
                    ( scenario, Cmd.none )

        ExperimentChanged eMsg ->
            ( { scenario | proposedExperiment = adapter.logic.updateExperiment eMsg scenario.proposedExperiment }, Cmd.none )

        GuessChanged gMsg ->
            ( { scenario | proposedGuess = adapter.logic.updateGuess gMsg scenario.proposedGuess }, Cmd.none )

        MakeGuess ->
            case scenario.history of
                active :: rest ->
                    ( { scenario
                        | history = { active | guess = Just scenario.proposedGuess } :: rest
                        , proposedGuess = adapter.init.defaultGuess
                      }
                    , Cmd.none
                    )

                -- TODO update challenge state
                _ ->
                    ( scenario, Cmd.none )

        NewInstance ->
            ( scenario, Random.generate SpecGenerated (instanceGenerator adapter.logic.specGenerator) )


instanceGenerator : Random.Generator spec -> Random.Generator ( String, spec )
instanceGenerator specGenerator =
    Random.map2 Tuple.pair Names.creatureNameGenerator specGenerator


allowMoreExperiments : Instance spec experiment outcome guess -> Bool
allowMoreExperiments instance =
    List.length instance.data < 100


view :
    Adapter expMsg guessMsg spec experiment outcome guess
    -> Scenario spec experiment outcome guess
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
view adapter scenario =
    div [ Attr.class "scenario" ]
        [ viewStats adapter scenario
        , viewGameControls adapter scenario
        , viewHistory adapter scenario.history
        , div [ Attr.class "scenarioFooter" ] []
        ]


getResults :
    LogicAdapter expMsg guessMsg spec experiment outcome guess
    -> List (HistoryItem spec experiment outcome guess)
    -> List ( Bool, Int )
getResults adapter history =
    case history of
        head :: rest ->
            case head.guess of
                Just guess ->
                    let
                        correct =
                            Tuple.first (adapter.guessEval head.spec guess)
                    in
                    ( correct, computeCost adapter head )
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
            correct |> List.map Utils.boolToInt |> Utils.safeAverage

        propCorrectShort =
            correctShort |> List.map Utils.boolToInt |> Utils.safeAverage

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
                    , text (Round.round 1 (propCorrect * 100) ++ "% correct, avg cost: CZK " ++ String.fromInt (round avgCost))
                    , br [] []
                    , strong [] [ text "Last ", text (String.fromInt adapter.init.instancesToAverage), text " instances: " ]
                    , if nRes > adapter.init.instancesToAverage then
                        text (Round.round 1 (propCorrectShort * 100) ++ "% correct, avg cost: CZK " ++ String.fromInt (round avgCostShort))

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
                , div [ Attr.class "scenarioControl" ] [ viewScenarioControl adapter scenario ]
                , case scenario.history of
                    activeInstance :: _ ->
                        h3 []
                            [ text ("Instance " ++ String.fromInt (List.length scenario.history) ++ ": ")
                            , em [] [ text activeInstance.creatureName ]
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
    Adapter expMsg guessMsg spec experiment outcome guess
    -> List (HistoryItem spec experiment outcome guess)
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
viewHistory adapter historyList =
    case historyList of
        head :: rest ->
            let
                activeId =
                    List.length historyList - 1

                activeScenario =
                    viewSingleHistory adapter True activeId head

                breakElement =
                    case rest of
                        _ :: _ ->
                            h2 [ Attr.class "historyStart" ] [ text "Previous instances " ]

                        [] ->
                            text ""

                history =
                    reverseIndexedMap (viewSingleHistory adapter False) rest
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
    Adapter expMsg guessMsg spec experiment outcome guess
    -> Bool
    -> Int
    -> HistoryItem spec experiment outcome guess
    -> Html (Msg expMsg guessMsg spec experiment outcome guess)
viewSingleHistory adapter active id instance =
    let
        experiments =
            reverseIndexedMap (adapter.view.viewExperiment instance.spec) instance.data
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
                                    (if correct then
                                        "CORRECT"

                                     else
                                        "INCORRECT"
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
        , dataDesc
        , Html.Keyed.node "div" [ Attr.class "experiments" ] (withReverseIds experiments)
        ]
