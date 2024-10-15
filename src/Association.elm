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
    , association : Bool
    , contrib : Float
    }


type alias Experiment =
    Int


type alias Guess =
    Bool


type alias Outcome =
    VL.Spec


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
    { defaultGuess = False
    , defaultExperiment = 100
    , scenarioName = "Is there an association?"
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
    { viewExperiment = viewExperiment
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
        assoc =
            Random.uniform True [ False ]

        contrib =
            Causality.contribGenerator

        varNames =
            [ "A", "B" ]

        variables =
            Random.list 2 Causality.interceptGenerator
                |> Random.map (\intercepts -> List.map2 Causality.Variable varNames intercepts)

        edgeListFromAssoc =
            \assocVal contribVal ->
                if assocVal then
                    [ { from = 0, to = 1, label = contribVal } ]

                else
                    []

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
            \assocVal contribVal vars ->
                { sorted = sortedFromCausesAndVars assocVal contribVal vars
                , association = assocVal
                , contrib = contribVal
                }
    in
    Random.map3 specFromData assoc contrib variables


generator : Spec -> Experiment -> Random.Generator VL.Spec
generator spec experiment =
    let
        outcome =
            Causality.generatorObservational spec.sorted experiment
    in
    Random.map (Causality.outcomeToSpec (List.map .name spec.sorted.variables)) outcome


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
        , if spec.association then
            span []
                [ text "When "
                , em [] [ text name0 ]
                , text " is true "
                , em [] [ text name1 ]
                , text " is "
                , text
                    (if spec.contrib > 0 then
                        "more likely"

                     else
                        "less likely"
                    )
                ]

          else
            span [] [ text "There is no association." ]
        )


updateExperiment : ExpMsg -> Experiment -> Experiment
updateExperiment msg experiment =
    case msg of
        SetN newN ->
            case String.toInt newN of
                Just n ->
                    n

                Nothing ->
                    experiment


updateGuess : GuessMsg -> Guess -> Guess
updateGuess msg _ =
    case msg of
        SetGuess g ->
            g


viewExperiment : Spec -> ( Experiment, Outcome ) -> Html Never
viewExperiment _ ( experiment, data ) =
    div []
        [ div [] [ text ("N = " ++ String.fromInt experiment ++ ", CZK " ++ String.fromInt (costExperiment experiment)) ]
        , Html.Lazy.lazy View.vegaPlot data
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
viewGuess _ guess =
    let
        guessDescription =
            if guess then
                "There IS an association"

            else
                "NO association"
    in
    h3 [] [ text ("Your guess: " ++ guessDescription) ]


viewProposedGuess : Spec -> Guess -> Html GuessMsg
viewProposedGuess spec guess =
    let
        ( name0, name1 ) =
            specToNames spec
    in
    div []
        [ text "I believe "
        , em [] [ text name0 ]
        , text " "
        , select []
            [ option [ Attr.selected guess, Events.onClick (SetGuess True) ] [ text "IS associated" ]
            , option [ Attr.selected (not guess), Events.onClick (SetGuess False) ] [ text "is NOT associated" ]
            ]
        , text " with "
        , em [] [ text name1 ]
        , text "."
        ]
