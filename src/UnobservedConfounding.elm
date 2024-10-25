module UnobservedConfounding exposing (..)

import Causality
import Game
import Graph
import Html exposing (..)
import Html.Attributes as Attr
import Random
import Utils


type alias Spec =
    { sorted : Causality.SortedDAG
    , causeUnobs0 : Causality.Category
    , causeUnobs1 : Causality.Category
    , cause01 : Causality.Category
    }


type alias Experiment =
    Causality.Experiment


type alias Guess =
    Causality.Category


type alias Outcome =
    Causality.Outcome


type alias ExpMsg =
    Causality.ExpMsg


type GuessMsg
    = SetCause Causality.Category


type alias Msg =
    Game.Msg ExpMsg GuessMsg Spec Experiment Outcome Guess


type alias Model =
    Game.Scenario Spec Experiment Outcome Guess


adapter : Game.Adapter ExpMsg GuessMsg Spec Experiment Outcome Guess
adapter =
    { init =
        { defaultGuess = Causality.NoCause
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
        , viewInstanceGoal = always (text "")
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
        unobsCauseGenerator =
            Random.uniform Causality.RightNeg [ Causality.RightPos ]

        causesContribs =
            Random.map3 Utils.triplet Causality.categoryGenerator unobsCauseGenerator unobsCauseGenerator
                |> Random.andThen (\( c1, c2, c3 ) -> Random.map (Tuple.pair ( c1, c2, c3 )) (Random.map3 Utils.triplet (Causality.contribGenerator c1) (Causality.contribGenerator c2) (Causality.contribGenerator c3)))

        varNames =
            [ "A", "B", "Unobserved" ]

        variables =
            Random.list 3 Causality.interceptGenerator
                |> Random.map (\intercepts -> List.map2 Causality.Variable varNames intercepts)

        graphFromCauses =
            \( cause01, causeUnobs0, causeUnobs1 ) ( contrib01, contribUnobs0, contribUnobs1 ) ->
                Graph.fromNodesAndEdges
                    (List.map (\x -> Graph.Node x x) [ 0, 1, 2 ])
                    (Causality.edgeListFromCause 0 1 cause01 contrib01
                        ++ Causality.edgeListFromCause 2 0 causeUnobs0 contribUnobs0
                        ++ Causality.edgeListFromCause 2 1 causeUnobs1 contribUnobs1
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
            \( ( cause01, causeUnobs0, causeUnobs1 ), contribVals ) vars ->
                { sorted = sortedFromCausesAndVars ( cause01, causeUnobs0, causeUnobs1 ) contribVals vars
                , cause01 = cause01
                , causeUnobs0 = causeUnobs0
                , causeUnobs1 = causeUnobs1
                }
    in
    Random.map2 specFromData causesContribs variables


generator : Spec -> Experiment -> Random.Generator Outcome
generator spec experiment =
    Causality.outcomeGenerator spec.sorted experiment


guessEval : Spec -> Guess -> Game.GuessEval
guessEval spec guess =
    let
        ( name0, name1, nameUnobs ) =
            specToNames spec
    in
    if guess == spec.cause01 then
        ( 1.0, text "" )

    else
        ( 0.0
        , div []
            [ Causality.causalityDescription name0 name1 spec.cause01
            , Causality.causalityDescription nameUnobs name0 spec.causeUnobs0
            , Causality.causalityDescription nameUnobs name1 spec.causeUnobs1
            ]
        )


updateGuess : GuessMsg -> Guess -> Guess
updateGuess msg _ =
    case msg of
        SetCause g ->
            g


viewExperiment : Spec -> Int -> ( Experiment, Outcome ) -> Html Never
viewExperiment spec id ( experiment, outcome ) =
    let
        filteredOutcome =
            List.take 2 outcome
    in
    Causality.viewExperiment spec.sorted id ( experiment, filteredOutcome )


viewProposedExperiment : Spec -> Experiment -> Html ExpMsg
viewProposedExperiment spec =
    let
        sorted =
            spec.sorted

        filteredSorted =
            { sorted | variables = List.take 2 spec.sorted.variables }
    in
    Causality.viewProposedExperiment filteredSorted


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
        ( name0, name1, _ ) =
            specToNames spec
    in
    div []
        [ Causality.causalityDescription name0 name1 guess
        ]


viewProposedGuess : Spec -> Guess -> Html GuessMsg
viewProposedGuess spec guess =
    let
        ( name0, name1, _ ) =
            specToNames spec
    in
    div []
        [ text "I believe "
        , Causality.causalityProposedGuess name0 name1 SetCause guess
        ]


viewHeader : Html Never
viewHeader =
    div [ Attr.class "scenarioHeader" ]
        [ h2 [] [ text "Two way causality" ]
        ]
