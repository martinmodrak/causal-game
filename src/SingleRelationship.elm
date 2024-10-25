module SingleRelationship exposing (..)

import Association
import Causality
import Game exposing (GuessEval, Msg(..))
import Graph
import Html exposing (..)
import Html.Attributes as Attr
import Names
import Random


type alias Spec =
    { sorted : Causality.SortedDAG
    , category : Causality.Category
    }


type alias Experiment =
    Causality.Experiment


type alias Guess =
    Causality.Category


type alias Outcome =
    Causality.Outcome


type alias ExpMsg =
    Causality.ExpMsg


type alias GuessMsg =
    Association.GuessMsg


type alias Msg =
    Game.Msg ExpMsg GuessMsg Spec Experiment Outcome Guess


type alias Model =
    Game.Scenario Spec Experiment Outcome Guess


initAdapter : Game.InitAdapter Guess Experiment
initAdapter =
    { defaultGuess = Causality.NoCause
    , defaultExperiment = { randomized = False, n = 100, intervention = 0 }
    , instancesToAverage = 4
    }


logicAdapter : Game.LogicAdapter ExpMsg GuessMsg Spec Experiment Outcome Guess
logicAdapter =
    { specGenerator = specGenerator
    , generator = generator
    , updateExperiment = Causality.updateExperiment
    , updateGuess = Association.updateGuess
    , guessEval = guessEval
    , costExperiment = Causality.costExperiment
    }


viewAdapter : Game.ViewAdapter ExpMsg GuessMsg Spec Experiment Outcome Guess
viewAdapter =
    { viewHeader = viewHeader
    , viewInstanceGoal = viewInstanceGoal
    , viewExperiment = viewExperiment
    , viewProposedExperiment = viewProposedExperiment
    , viewCostCommentary = Causality.viewCostCommentary
    , viewGuess = viewGuess
    , viewProposedGuess = viewProposedGuess
    }


adapter : Game.Adapter ExpMsg GuessMsg Spec Experiment Outcome Guess
adapter =
    { init = initAdapter
    , logic =
        logicAdapter
    , view = viewAdapter
    }


specGenerator : Random.Generator Spec
specGenerator =
    let
        causeContrib =
            Causality.categoryGenerator
                |> Random.andThen (\assocVal -> Random.map (Tuple.pair assocVal) (Causality.contribGenerator assocVal))

        varNamesGen =
            Names.nameGenerator 2

        variables =
            Random.map2
                (\varNames intercepts -> List.map2 Causality.Variable varNames intercepts)
                varNamesGen
                (Random.list 2 Causality.interceptGenerator)

        edgeListFromAssoc =
            \assocVal contribVal ->
                case assocVal of
                    Causality.NoCause ->
                        []

                    Causality.RightPos ->
                        [ { from = 0, to = 1, label = contribVal } ]

                    Causality.RightNeg ->
                        [ { from = 0, to = 1, label = contribVal } ]

                    Causality.LeftPos ->
                        [ { from = 1, to = 0, label = contribVal } ]

                    Causality.LeftNeg ->
                        [ { from = 1, to = 0, label = contribVal } ]

        graphFromCauses =
            \assocVal contribVal ->
                Graph.fromNodesAndEdges
                    (List.map (\x -> Graph.Node x x) [ 0, 1 ])
                    (edgeListFromAssoc assocVal contribVal)

        sortedFromCausesAndVars =
            \assocVal contribVal vars ->
                { variables = vars
                , sorted =
                    case Graph.checkAcyclic (graphFromCauses assocVal contribVal) of
                        Ok dag ->
                            Graph.topologicalSort dag

                        Err _ ->
                            []
                }

        specFromData =
            \( assocVal, contribVal ) vars ->
                { sorted = sortedFromCausesAndVars assocVal contribVal vars
                , category = assocVal
                }
    in
    Random.map2 specFromData causeContrib variables


generator : Spec -> Experiment -> Random.Generator Outcome
generator spec experiment =
    Causality.outcomeGenerator spec.sorted experiment


specToNames : Spec -> ( String, String )
specToNames spec =
    case spec.sorted.variables of
        a :: b :: _ ->
            ( a.name, b.name )

        _ ->
            ( "Error", "Error" )


guessEval : Spec -> Guess -> GuessEval
guessEval spec guess =
    let
        ( name0, name1 ) =
            specToNames spec
    in
    if guess == spec.category then
        ( 1.0, text "" )

    else
        ( 0.0
        , Causality.causalityDescription name0 name1 spec.category
        )


viewExperiment : Spec -> Int -> ( Experiment, Outcome ) -> Html Never
viewExperiment spec =
    Causality.viewExperiment spec.sorted


viewProposedExperiment : Spec -> Experiment -> Html ExpMsg
viewProposedExperiment spec =
    Causality.viewProposedExperiment spec.sorted


viewGuess : Spec -> Guess -> Html Never
viewGuess spec guess =
    let
        ( name0, name1 ) =
            specToNames spec
    in
    div []
        [ Causality.causalityDescription name0 name1 guess
        ]


viewProposedGuess : Spec -> Guess -> Html GuessMsg
viewProposedGuess spec guess =
    let
        ( name0, name1 ) =
            specToNames spec
    in
    div []
        [ text "I believe "
        , Causality.causalityProposedGuess name0 name1 Association.SetGuess guess
        ]


viewHeader : Html Never
viewHeader =
    div [ Attr.class "scenarioHeader" ]
        [ h2 [] [ text "Single causal relationship" ]
        , p [] [ strong [] [ text "This scenario does not contribute to homework scoring, it is there to help you learn how the game works." ] ]
        , p []
            [ text "Here, the change from the previous scenario is that now the direction of causality is not given and you have to find it yourself."
            , text " You can now also access randomized experiments that help you do exactly that. You mess up with the aliens, such that after randomization the randomized variable will be"
            , text " randomly split 50:50 between true and false. If there is an association between a randomized variable and another one, it has to be causal."
            , text " Since we only have two variables, the situation is simple and any causation has to be direct, but if we investigate many variables, indirect causal relationships will also result in an associaton."
            , text " The problem with randomized experiments is that they are really expensive. So it might make sense to try an observational study before investing in a randomized one."
            ]
        , p []
            [ text " You should be able to obtain almost 100% accuracy while spending on average at most CZK 200 000 "
            ]
        ]


viewInstanceGoal : Spec -> Html Never
viewInstanceGoal spec =
    let
        ( name1, name2 ) =
            specToNames spec
    in
    span [] [ text "Investigate a possible causal relationship between traits ", em [] [ text name1 ], text " and ", em [] [ text name2 ] ]
