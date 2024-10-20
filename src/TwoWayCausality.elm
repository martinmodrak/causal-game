module TwoWayCausality exposing (..)

import Causality
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


type ExpMsg
    = SetN String
    | SetRandomized Bool
    | SetIntervention Int


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
        , defaultExperiment = { randomized = True, n = 500, intervention = 1 }
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
        causesContribs =
            Random.map2 Tuple.pair Causality.categoryGenerator Causality.categoryGenerator
                |> Random.andThen (\( c1, c2 ) -> Random.map (Tuple.pair ( c1, c2 )) (Random.map2 Tuple.pair (Causality.contribGenerator c1) (Causality.contribGenerator c2)))

        varNames =
            [ "A", "B", "C" ]

        variables =
            Random.list 3 Causality.interceptGenerator
                |> Random.map (\intercepts -> List.map2 Causality.Variable varNames intercepts)

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


generator : Spec -> Experiment -> Random.Generator Outcome
generator spec experiment =
    if experiment.randomized then
        Causality.generatorRandomized spec.sorted experiment.intervention experiment.n

    else
        Causality.generatorObservational spec.sorted experiment.n


guessEval : Spec -> Guess -> GuessEval
guessEval spec guess =
    let
        ( name0, name1, name2 ) =
            specToNames spec
    in
    if guess.cause01 == spec.cause01 && guess.cause12 == spec.cause12 then
        ( True, text "" )

    else
        ( False
        , div []
            [ Causality.causalityDescription name0 name1 spec.cause01
            , br [] []
            , Causality.causalityDescription name1 name2 spec.cause12
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
                    if experiment.intervention == Causality.noIntervention then
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
        SetCause01 g ->
            { old | cause01 = g }

        SetCause12 g ->
            { old | cause12 = g }


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
            , br [] []
            , text ("N = " ++ String.fromInt experiment.n ++ ", CZK " ++ String.fromInt (costExperiment experiment))
            ]
        , Html.Lazy.lazy2 Causality.viewOutcome spec.sorted data
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
        ( name0, name1, name2 ) =
            specToNames spec
    in
    div []
        [ h3 []
            [ text "Your guess: " ]
        , Causality.causalityDescription name0 name1 guess.cause01
        , br [] []
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
        , text "."
        ]
