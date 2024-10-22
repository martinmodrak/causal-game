module Association exposing (..)

import Causality
import Game exposing (GuessEval, Msg(..))
import Graph
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy
import Random
import VegaLite as VL
import View


type alias Spec =
    { sorted : Causality.SortedDAG
    , association : Causality.Category
    , contrib : Float
    }


type alias Experiment =
    Int


type alias Guess =
    Causality.Category


type alias Outcome =
    Causality.Outcome


type ExpMsg
    = SetN String


type GuessMsg
    = SetGuess Guess


type alias Msg =
    Game.Msg ExpMsg GuessMsg Spec Experiment Outcome Guess


type alias Model =
    Game.Scenario Spec Experiment Outcome Guess


initAdapter : Game.InitAdapter Guess Experiment
initAdapter =
    { defaultGuess = Causality.NoCause
    , defaultExperiment = 100
    , instancesToAverage = 4
    }


logicAdapter : Game.LogicAdapter ExpMsg GuessMsg Spec Experiment Outcome Guess
logicAdapter =
    { specGenerator = specGenerator
    , generator = generator
    , updateExperiment = updateExperiment
    , updateGuess = updateGuess
    , guessEval = guessEval
    , costExperiment = costExperiment
    }


viewAdapter : Game.ViewAdapter ExpMsg GuessMsg Spec Experiment Outcome Guess
viewAdapter =
    { viewHeader = viewHeader
    , viewExperiment = viewExperiment
    , viewProposedExperiment = viewProposedExperiment
    , viewCostCommentary = viewCostCommentary
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
        assocContrib =
            Random.uniform Causality.NoCause [ Causality.RightPos, Causality.RightNeg ]
                |> Random.andThen (\assocVal -> Random.map (Tuple.pair assocVal) (Causality.contribGenerator assocVal))

        varNames =
            [ "A", "B" ]

        variables =
            Random.list 2 Causality.interceptGenerator
                |> Random.map (\intercepts -> List.map2 Causality.Variable varNames intercepts)

        edgeListFromAssoc =
            \assocVal contribVal ->
                case assocVal of
                    Causality.NoCause ->
                        []

                    _ ->
                        [ { from = 0, to = 1, label = contribVal } ]

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
                , association = assocVal
                , contrib = contribVal
                }
    in
    Random.map2 specFromData assocContrib variables


generator : Spec -> Experiment -> Random.Generator Outcome
generator spec experiment =
    Causality.generatorObservational spec.sorted experiment


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
    if guess == spec.association then
        ( True, text "" )

    else
        ( False
        , Causality.causalityDescription name0 name1 spec.association
        )


updateExperiment : ExpMsg -> Experiment -> Experiment
updateExperiment msg experiment =
    case msg of
        SetN newN ->
            case String.toInt newN of
                Just n ->
                    min n Causality.maxN

                Nothing ->
                    experiment


updateGuess : GuessMsg -> Guess -> Guess
updateGuess msg _ =
    case msg of
        SetGuess g ->
            g


viewExperiment : Spec -> Int -> ( Experiment, Outcome ) -> Html Never
viewExperiment spec id ( experiment, data ) =
    div [ Attr.class "experiment" ]
        [ View.experimentTitle id
        , p [] [ text ("N = " ++ String.fromInt experiment ++ ", CZK " ++ String.fromInt (costExperiment experiment)) ]
        , Html.Lazy.lazy2 Causality.viewOutcome spec.sorted data
        ]


viewProposedExperiment : Spec -> Experiment -> Html ExpMsg
viewProposedExperiment _ experiment =
    div []
        [ text "Run an observational study with "
        , View.nChooser SetN experiment
        , text " participants."
        ]


costPerParticipant : Int
costPerParticipant =
    100


costPerExperiment : Int
costPerExperiment =
    2000


costExperiment : Experiment -> Int
costExperiment exp =
    exp * costPerParticipant + costPerExperiment


viewCostCommentary : Html a
viewCostCommentary =
    text ("Observational study costs CZK " ++ String.fromInt costPerExperiment ++ " + CZK " ++ String.fromInt costPerParticipant ++ " per participant")


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

        singleOption =
            \val ->
                option [ Attr.selected (guess == val), Attr.value (Causality.causalityDirectionToShortString val) ] [ text (Causality.causalityDirectionToString val) ]
    in
    div []
        [ text "I believe "
        , em [] [ text name0 ]
        , text " "
        , select [ View.onChange (Causality.causalityDirectionFromShortString >> SetGuess) ]
            [ singleOption Causality.NoCause
            , singleOption Causality.RightPos
            , singleOption Causality.RightNeg
            ]
        , text " "
        , em [] [ text name1 ]
        , text "."
        ]


viewHeader : Html Never
viewHeader =
    div [ Attr.class "scenarioHeader" ]
        [ h2 [] [ text "Is there an association?" ]
        , p [] [ text "" ]
        ]
