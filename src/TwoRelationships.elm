module TwoRelationships exposing (..)

import Causality
import Game
import Graph
import Html exposing (..)
import Html.Attributes as Attr
import Names
import Random
import Utils


type alias Spec =
    { sorted : Causality.SortedDAG
    , cause01 : Causality.Category
    , cause12 : Causality.Category
    }


type alias Experiment =
    Causality.Experiment


type alias Guess =
    { cause01 : Causality.Category
    , cause12 : Causality.Category
    }


type alias Outcome =
    Causality.Outcome


type alias ExpMsg =
    Causality.ExpMsg


type GuessMsg
    = SetCause01 Causality.Category
    | SetCause12 Causality.Category


type alias Msg =
    Game.Msg ExpMsg GuessMsg Spec Experiment Outcome Guess


type alias Model =
    Game.Scenario Spec Experiment Outcome Guess


adapter : Game.Adapter ExpMsg GuessMsg Spec Experiment Outcome Guess
adapter =
    { init =
        { defaultGuess = { cause01 = Causality.NoCause, cause12 = Causality.NoCause }
        , defaultExperiment = { randomized = False, n = 100, intervention = 0 }
        , instancesToAverage = 3
        }
    , logic =
        { specGenerator = specGenerator
        , generator = generator
        , updateExperiment = Causality.updateExperiment
        , updateGuess = updateGuess
        , guessEval = guessEval
        , costExperiment = Causality.costExperiment
        }
    , view =
        { viewHeader = viewHeader
        , viewInstanceGoal = viewInstanceGoal
        , viewExperiment = viewExperiment
        , viewProposedExperiment = viewProposedExperiment
        , viewCostCommentary = Causality.viewCostCommentary
        , viewGuess = viewGuess
        , viewProposedGuess = viewProposedGuess
        }
    }


specGenerator : Random.Generator Spec
specGenerator =
    let
        causesContribs =
            Random.map2 Tuple.pair Causality.categoryGenerator Causality.categoryGenerator
                |> Random.andThen (\( c1, c2 ) -> Random.map (Tuple.pair ( c1, c2 )) (Random.map2 Tuple.pair (Causality.contribGenerator c1) (Causality.contribGenerator c2)))

        varNamesGen =
            Names.nameGenerator 3

        variables =
            Random.map2
                (\varNames intercepts -> List.map2 Causality.Variable varNames intercepts)
                varNamesGen
                (Random.list 3 Causality.interceptGenerator)

        edgeListFromCause =
            \id1 id2 cause contrib ->
                case cause of
                    Causality.NoCause ->
                        []

                    Causality.RightPos ->
                        [ { from = id1, to = id2, label = contrib } ]

                    Causality.RightNeg ->
                        [ { from = id1, to = id2, label = contrib } ]

                    Causality.LeftPos ->
                        [ { from = id2, to = id1, label = contrib } ]

                    Causality.LeftNeg ->
                        [ { from = id2, to = id1, label = contrib } ]

        graphFromCauses =
            \( cause01, cause12 ) ( contrib01, contrib12 ) ->
                Graph.fromNodesAndEdges
                    (List.map (\x -> Graph.Node x x) [ 0, 1, 2 ])
                    (edgeListFromCause 0 1 cause01 contrib01
                        ++ edgeListFromCause 1 2 cause12 contrib12
                    )

        sortedFromCausesAndVars =
            \causesVals contribVals vars ->
                { variables = vars
                , sorted =
                    case Graph.checkAcyclic (graphFromCauses causesVals contribVals) of
                        Ok dag ->
                            Graph.topologicalSort dag

                        Err _ ->
                            []
                }

        specFromData =
            \( ( cause01, cause12 ), contribVals ) vars ->
                { sorted = sortedFromCausesAndVars ( cause01, cause12 ) contribVals vars
                , cause01 = cause01
                , cause12 = cause12
                }
    in
    Random.map2 specFromData causesContribs variables


generator : Spec -> Experiment -> Random.Generator Outcome
generator spec experiment =
    Causality.outcomeGenerator spec.sorted experiment


guessEval : Spec -> Guess -> Game.GuessEval
guessEval spec guess =
    let
        ( name0, name1, name2 ) =
            specToNames spec

        correct01 =
            guess.cause01 == spec.cause01

        correct02 =
            guess.cause12 == spec.cause12

        corectTotal =
            0.5 * (Utils.boolToFloat correct01 + Utils.boolToFloat correct02)
    in
    if correct01 && correct02 then
        ( 1.0, text "" )

    else
        ( corectTotal
        , div []
            [ Causality.causalityDescription name0 name1 spec.cause01
            , Causality.causalityDescription name1 name2 spec.cause12
            ]
        )


updateGuess : GuessMsg -> Guess -> Guess
updateGuess msg old =
    case msg of
        SetCause01 g ->
            { old | cause01 = g }

        SetCause12 g ->
            { old | cause12 = g }


viewExperiment : Game.ViewSettings -> Spec -> Int -> ( Experiment, Outcome ) -> Html Never
viewExperiment viewSettings spec =
    Causality.viewExperiment viewSettings spec.sorted


viewProposedExperiment : Spec -> Experiment -> Html ExpMsg
viewProposedExperiment spec =
    Causality.viewProposedExperiment spec.sorted


specToNames : Spec -> ( String, String, String )
specToNames spec =
    case spec.sorted.variables of
        a :: b :: c :: _ ->
            ( a.name, b.name, c.name )

        _ ->
            ( "Error", "Error", "Error" )


viewGuess : Spec -> Guess -> Html Never
viewGuess spec guess =
    let
        ( name0, name1, name2 ) =
            specToNames spec
    in
    div []
        [ Causality.causalityDescription name0 name1 guess.cause01
        , Causality.causalityDescription name1 name2 guess.cause12
        ]


viewProposedGuess : Spec -> Guess -> Html GuessMsg
viewProposedGuess spec guess =
    let
        ( name0, name1, name2 ) =
            specToNames spec
    in
    div []
        [ text "I believe "
        , Causality.causalityProposedGuess name0 name1 SetCause01 guess.cause01
        , text " AND "
        , Causality.causalityProposedGuess name1 name2 SetCause12 guess.cause12
        ]


viewHeader : Html Never
viewHeader =
    div [ Attr.class "scenarioHeader" ]
        [ h2 [] [ text "Two way causality" ]
        , em [] [ text "We strongly recommend trying out the previous scenarios before going here." ]
        , p []
            [ text "Here we add another variable, but to make things manageable, you know that only two relationships are possible (as noted below). "
            , text " This is the actual homework stuff. Good luck."
            ]
        ]


viewInstanceGoal : Spec -> Html Never
viewInstanceGoal spec =
    let
        ( name1, name2, name3 ) =
            specToNames spec
    in
    span []
        [ text "Investigate a possible causal relationship between traits "
        , em [] [ text name1 ]
        , text " and "
        , em [] [ text name2 ]
        , text " as well as between "
        , em [] [ text name2 ]
        , text " and "
        , em [] [ text name3 ]
        , text "."
        , br [] []
        , text "You can be sure that there is not causal relationship between "
        , em [] [ text name1 ]
        , text " and "
        , em [] [ text name3 ]
        ]
