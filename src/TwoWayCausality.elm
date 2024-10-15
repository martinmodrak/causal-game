module TwoWayCausality exposing (..)

import Causality exposing (CauseDirection(..), SortedDAG, Variable, causalityDescription, causeGenerator, contribGenerator, noIntervention, outcomeToSpec)
import Game exposing (GuessEval, Msg(..))
import Graph
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy
import Random
import Random.Extra
import Random.Float
import Round
import VegaLite as VL
import View


type alias Spec =
    { sorted : SortedDAG
    , cause12 : CauseDirection
    , cause23 : CauseDirection
    }


type alias Experiment =
    Causality.Experiment


type alias Guess =
    { cause12 : CauseDirection
    , cause23 : CauseDirection
    }


type alias Outcome =
    VL.Spec


type ExpMsg
    = SetN String
    | SetRandomized Bool
    | SetIntervention Int


type GuessMsg
    = SetCause12 CauseDirection
    | SetCause23 CauseDirection


type alias Msg =
    Game.Msg ExpMsg GuessMsg Spec Experiment Outcome Guess


type alias Model =
    Game.Scenario Spec Experiment Outcome Guess


adapter : Game.Adapter ExpMsg GuessMsg Spec Experiment Outcome Guess
adapter =
    { init =
        { defaultGuess = { cause12 = NoCause, cause23 = NoCause }
        , defaultExperiment = { randomized = True, n = 50, intervention = 1 }
        , scenarioName = "Two way causality"
        }
    , logic =
        { specGenerator = specGenerator
        , generator = generator
        , updateExperiment = updateExperiment
        , updateGuess = updateGuess
        , guessEval = guessEval
        , costExperiment = costExperiment
        }
    , view =
        { viewExperiment = viewExperiment
        , viewProposedExperiment = viewProposedExperiment
        , viewCostCommentary = viewCostCommentary
        , viewGuess = viewGuess
        , viewProposedGuess = viewProposedGuess
        }
    }


specGenerator : Random.Generator Spec
specGenerator =
    let
        causes =
            Random.map2 Tuple.pair causeGenerator causeGenerator

        varNames =
            [ "A", "B", "C" ]

        variables =
            Random.list 3 (Random.float -1 1)
                |> Random.map (\intercepts -> List.map2 Causality.Variable varNames intercepts)

        contribs =
            Random.map2 Tuple.pair Causality.contribGenerator Causality.contribGenerator

        edgeListFromCause =
            \id1 id2 cause contrib ->
                case cause of
                    NoCause ->
                        []

                    Right ->
                        [ { from = id1, to = id2, label = contrib } ]

                    Left ->
                        [ { from = id2, to = id1, label = contrib } ]

        graphFromCauses =
            \( cause12, cause23 ) ( contrib12, contrib23 ) ->
                Graph.fromNodesAndEdges
                    (List.map (\x -> Graph.Node x x) [ 1, 2, 3 ])
                    (edgeListFromCause 1 2 cause12 contrib12
                        ++ edgeListFromCause 2 2 cause23 contrib23
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
            \( cause12, cause23 ) contribVals vars ->
                { sorted = sortedFromCausesAndVars ( cause12, cause23 ) contribVals vars
                , cause12 = cause12
                , cause23 = cause23
                }
    in
    Random.map3 specFromData causes contribs variables


maxAbsSlopeNoAssoc : Float
maxAbsSlopeNoAssoc =
    0.5


slopeGenerator : Random.Generator Float
slopeGenerator =
    let
        genNoAssoc =
            Random.float -maxAbsSlopeNoAssoc maxAbsSlopeNoAssoc

        genAssoc =
            Random.map2 (*) (Random.Extra.choice -1 1) (Random.float 2 6)
    in
    Random.Extra.choices genAssoc [ genNoAssoc ]


generator : Spec -> Experiment -> Random.Generator VL.Spec
generator spec experiment =
    let
        outcome =
            if experiment.randomized then
                Causality.generatorRandomized spec.sorted experiment.intervention experiment.n

            else
                Causality.generatorObservational spec.sorted experiment.n
    in
    Random.map (outcomeToSpec (List.map .name spec.sorted.variables)) outcome


guessEval : Spec -> Guess -> GuessEval
guessEval spec guess =
    let
        ( name1, name2, name3 ) =
            specToNames spec
    in
    if guess.cause12 == spec.cause12 && guess.cause23 == spec.cause23 then
        ( True, text "" )

    else
        ( False
        , div []
            [ causalityDescription name1 name2 spec.cause12
            , br [] []
            , causalityDescription name2 name3 spec.cause23
            ]
        )


updateExperiment : ExpMsg -> Experiment -> Experiment
updateExperiment msg experiment =
    case msg of
        SetN newN ->
            case String.toInt newN of
                Just n ->
                    { experiment | n = n }

                Nothing ->
                    experiment

        SetRandomized newRand ->
            let
                newIntervention =
                    if experiment.intervention == noIntervention then
                        1

                    else
                        experiment.intervention
            in
            { experiment | randomized = newRand, intervention = newIntervention }

        SetIntervention newIntervention ->
            { experiment | intervention = newIntervention }


updateGuess : GuessMsg -> Guess -> Guess
updateGuess msg old =
    case msg of
        SetCause12 g ->
            { old | cause12 = g }

        SetCause23 g ->
            { old | cause23 = g }


viewExperiment : Spec -> ( Experiment, Outcome ) -> Html Never
viewExperiment spec ( experiment, data ) =
    let
        typeText =
            if experiment.randomized then
                "Randomizing "
                    -- TODO mess
                    ++ (case List.indexedMap Tuple.pair spec.sorted.variables |> List.filter (\( id, _ ) -> id == experiment.intervention) of
                            [] ->
                                "Error"

                            ( _, var ) :: _ ->
                                var.name
                       )

            else
                "Observational study"
    in
    div []
        [ div []
            [ strong [] [ text typeText ]
            , text ("N = " ++ String.fromInt experiment.n ++ ", CZK " ++ String.fromInt (costExperiment experiment))
            ]
        , Html.Lazy.lazy View.vegaPlot data
        ]


viewProposedExperiment : Spec -> Experiment -> Html ExpMsg
viewProposedExperiment spec experiment =
    let
        randomizedOption =
            \val label ->
                option [ Attr.selected (experiment.randomized == val), Events.onClick (SetRandomized val) ] [ text label ]

        interventionOption =
            \id var ->
                option [ Attr.selected (experiment.intervention == id), Events.onClick (SetIntervention id) ] [ text var.name ]

        intervention =
            if experiment.randomized then
                span []
                    [ text ", randomizing "
                    , select []
                        (List.indexedMap interventionOption spec.sorted.variables)
                    ]

            else
                text ""
    in
    div []
        [ text "Run an "
        , select []
            [ randomizedOption False "observational"
            , randomizedOption True "randomized"
            ]
        , text " study "
        , intervention
        , text " with "
        , View.nChooser SetN experiment.n
        , text " participants."
        ]


costPerParticipant : Bool -> Int
costPerParticipant randomized =
    if randomized then
        2000

    else
        100


costPerExperiment : Bool -> Int
costPerExperiment randomized =
    if randomized then
        50000

    else
        2000


costExperiment : Experiment -> Int
costExperiment exp =
    exp.n * costPerParticipant exp.randomized + costPerExperiment exp.randomized


viewCostCommentary : Html a
viewCostCommentary =
    p []
        [ text ("Observational study costs CZK " ++ String.fromInt (costPerExperiment False) ++ " + CZK " ++ String.fromInt (costPerParticipant False) ++ " per participant")
        , br [] []
        , text ("Randomized study costs CZK " ++ String.fromInt (costPerExperiment True) ++ " + CZK " ++ String.fromInt (costPerParticipant True) ++ " per participant")
        ]


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
        ( name1, name2, name3 ) =
            specToNames spec
    in
    div []
        [ h3 []
            [ text "Your guess: " ]
        , causalityDescription name1 name2 guess.cause12
        , br [] []
        , causalityDescription name2 name3 guess.cause23
        ]


viewProposedGuess : Spec -> Guess -> Html GuessMsg
viewProposedGuess spec guess =
    let
        ( name1, name2, name3 ) =
            specToNames spec
    in
    div []
        [ text "I believe "
        , Causality.causalityProposedGuess name1 name2 SetCause12 guess.cause12
        , text " AND "
        , Causality.causalityProposedGuess name2 name3 SetCause12 guess.cause12
        , text "."
        ]
