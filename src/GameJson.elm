module GameJson exposing (..)

import Association
import Causality
import Game
import Graph
import IntDict
import Json.Decode as D
import Json.Encode as E
import SingleRelationship
import TwoRelationships


associationScenarioEncoder :
    Game.Scenario Association.Spec Association.Experiment Association.Outcome Association.Guess
    -> E.Value
associationScenarioEncoder =
    scenarioEncoder associationSpecEncoder E.int categoryEncoder


associationScenarioDecoder : D.Decoder (Game.Scenario Association.Spec Association.Experiment Association.Outcome Association.Guess)
associationScenarioDecoder =
    scenarioDecoder associationSpecDecoder D.int categoryDecoder


singleRelScenarioEncoder :
    Game.Scenario SingleRelationship.Spec SingleRelationship.Experiment SingleRelationship.Outcome SingleRelationship.Guess
    -> E.Value
singleRelScenarioEncoder =
    scenarioEncoder singleRelSpecEncoder experimentEncoder categoryEncoder


singleRelScenarioDecoder : D.Decoder (Game.Scenario SingleRelationship.Spec SingleRelationship.Experiment SingleRelationship.Outcome SingleRelationship.Guess)
singleRelScenarioDecoder =
    scenarioDecoder singleRelSpecDecoder experimentDecoder categoryDecoder


twoRelScenarioEncoder :
    Game.Scenario TwoRelationships.Spec TwoRelationships.Experiment TwoRelationships.Outcome TwoRelationships.Guess
    -> E.Value
twoRelScenarioEncoder =
    scenarioEncoder twoRelSpecEncoder experimentEncoder twoRelGuessEncoder


twoRelScenarioDecoder : D.Decoder (Game.Scenario TwoRelationships.Spec TwoRelationships.Experiment TwoRelationships.Outcome TwoRelationships.Guess)
twoRelScenarioDecoder =
    scenarioDecoder twoRelSpecDecoder experimentDecoder twoRelGuessDecoder


scenarioEncoder :
    (spec -> E.Value)
    -> (experiment -> E.Value)
    -> (guess -> E.Value)
    -> Game.Scenario spec experiment Causality.Outcome guess
    -> E.Value
scenarioEncoder specEnc experimentEnc guessEnc scenario =
    E.object
        [ ( "history", E.list (instanceEncoder specEnc experimentEnc guessEnc) scenario.history )
        , ( "proposedExperiment", experimentEnc scenario.proposedExperiment )
        , ( "proposedGuess", guessEnc scenario.proposedGuess )
        ]


scenarioDecoder :
    D.Decoder spec
    -> D.Decoder experiment
    -> D.Decoder guess
    -> D.Decoder (Game.Scenario spec experiment Causality.Outcome guess)
scenarioDecoder specDec experimentDec guessDec =
    D.map3 Game.Scenario
        (D.field "history" (D.list (instanceDecoder specDec experimentDec guessDec)))
        (D.field "proposedExperiment" experimentDec)
        (D.field "proposedGuess" guessDec)


instanceEncoder :
    (spec -> E.Value)
    -> (experiment -> E.Value)
    -> (guess -> E.Value)
    -> Game.Instance spec experiment Causality.Outcome guess
    -> E.Value
instanceEncoder specEnc experimentEnc guessEnc instance =
    let
        ( dataExp, dataOut ) =
            List.unzip instance.data
    in
    E.object
        [ ( "spec", specEnc instance.spec )
        , ( "creatureName", E.string instance.creatureName )
        , ( "dataExp", E.list experimentEnc dataExp )
        , ( "dataOut", E.list outcomeEncoder dataOut )
        , ( "guess"
          , case instance.guess of
                Just g ->
                    guessEnc g

                Nothing ->
                    E.null
          )
        ]


instanceDecoder :
    D.Decoder spec
    -> D.Decoder experiment
    -> D.Decoder guess
    -> D.Decoder (Game.Instance spec experiment Causality.Outcome guess)
instanceDecoder specDec experimentDec guessDec =
    D.map4 Game.Instance
        (D.field "spec" specDec)
        (D.field "creatureName" D.string)
        (D.map2 (List.map2 Tuple.pair)
            (D.field "dataExp" (D.list experimentDec))
            (D.field "dataOut" (D.list outcomeDecoder))
        )
        (D.field "guess" (D.nullable guessDec))


outcomeEncoder : Causality.Outcome -> E.Value
outcomeEncoder outcome =
    E.list (E.list E.bool) outcome


outcomeDecoder : D.Decoder Causality.Outcome
outcomeDecoder =
    D.list (D.list D.bool)


associationSpecEncoder : Association.Spec -> E.Value
associationSpecEncoder spec =
    E.object
        [ ( "sorted", sortedDAGEncoder spec.sorted )
        , ( "association", categoryEncoder spec.association )
        ]


associationSpecDecoder : D.Decoder Association.Spec
associationSpecDecoder =
    D.map2 Association.Spec
        (D.field "sorted" sortedDagDecoder)
        (D.field "association" categoryDecoder)


singleRelSpecEncoder : SingleRelationship.Spec -> E.Value
singleRelSpecEncoder spec =
    E.object
        [ ( "sorted", sortedDAGEncoder spec.sorted )
        , ( "category", categoryEncoder spec.category )
        ]


singleRelSpecDecoder : D.Decoder SingleRelationship.Spec
singleRelSpecDecoder =
    D.map2 SingleRelationship.Spec
        (D.field "sorted" sortedDagDecoder)
        (D.field "category" categoryDecoder)


twoRelSpecEncoder : TwoRelationships.Spec -> E.Value
twoRelSpecEncoder spec =
    E.object
        [ ( "sorted", sortedDAGEncoder spec.sorted )
        , ( "cause01", categoryEncoder spec.cause01 )
        , ( "cause12", categoryEncoder spec.cause12 )
        ]


twoRelSpecDecoder : D.Decoder TwoRelationships.Spec
twoRelSpecDecoder =
    D.map3 TwoRelationships.Spec
        (D.field "sorted" sortedDagDecoder)
        (D.field "cause01" categoryDecoder)
        (D.field "cause12" categoryDecoder)


twoRelGuessEncoder : TwoRelationships.Guess -> E.Value
twoRelGuessEncoder guess =
    E.object
        [ ( "cause01", categoryEncoder guess.cause01 )
        , ( "cause12", categoryEncoder guess.cause12 )
        ]


twoRelGuessDecoder : D.Decoder TwoRelationships.Guess
twoRelGuessDecoder =
    D.map2 TwoRelationships.Guess
        (D.field "cause01" categoryDecoder)
        (D.field "cause12" categoryDecoder)


experimentEncoder : Causality.Experiment -> E.Value
experimentEncoder exp =
    E.object
        [ ( "randomized", E.bool exp.randomized )
        , ( "intervention", E.int exp.intervention )
        , ( "n", E.int exp.n )
        ]


experimentDecoder : D.Decoder Causality.Experiment
experimentDecoder =
    D.map3 Causality.Experiment
        (D.field "randomized" D.bool)
        (D.field "intervention" D.int)
        (D.field "n" D.int)


variableEncoder : Causality.Variable -> E.Value
variableEncoder var =
    E.object
        [ ( "name", E.string var.name )
        , ( "intercept", E.float var.intercept )
        ]


variableDecoder : D.Decoder Causality.Variable
variableDecoder =
    D.map2 Causality.Variable (D.field "name" D.string) (D.field "intercept" D.float)


sortedDAGEncoder : Causality.SortedDAG -> E.Value
sortedDAGEncoder dag =
    E.object
        [ ( "variables", E.list variableEncoder dag.variables )
        , ( "sorted", sortedVariablesEncoder dag.sorted )
        ]


sortedDagDecoder : D.Decoder Causality.SortedDAG
sortedDagDecoder =
    D.map2 Causality.SortedDAG
        (D.field "variables" (D.list variableDecoder))
        (D.field "sorted" sortedVariablesDecoder)


sortedVariablesEncoder : Causality.SortedVariables -> E.Value
sortedVariablesEncoder sVars =
    E.list
        (\x ->
            E.object
                [ ( "node", nodeEncoder x.node )
                , ( "incoming", adjacencyEncoder x.incoming )
                , ( "outgoing", adjacencyEncoder x.outgoing )
                ]
        )
        sVars


sortedVariablesDecoder : D.Decoder Causality.SortedVariables
sortedVariablesDecoder =
    D.list
        (D.map3 Graph.NodeContext
            (D.field "node" nodeDecoder)
            (D.field "incoming" adjacencyDecoder)
            (D.field "outgoing" adjacencyDecoder)
        )


nodeEncoder : Graph.Node Int -> E.Value
nodeEncoder node =
    E.object
        [ ( "id", E.int node.id )
        , ( "label", E.int node.label )
        ]


nodeDecoder : D.Decoder (Graph.Node Int)
nodeDecoder =
    D.map2 Graph.Node (D.field "id" D.int) (D.field "label" D.int)


adjacencyEncoder : Graph.Adjacency Float -> E.Value
adjacencyEncoder adj =
    E.list
        (\( key, val ) ->
            E.object
                [ ( "key", E.int key )
                , ( "val", E.float val )
                ]
        )
        (IntDict.toList adj)


adjacencyDecoder : D.Decoder (Graph.Adjacency Float)
adjacencyDecoder =
    D.list
        (D.map2
            Tuple.pair
            (D.field "key" D.int)
            (D.field "val" D.float)
        )
        |> D.map IntDict.fromList


categoryEncoder : Causality.Category -> E.Value
categoryEncoder cat =
    E.string (Causality.categoryToShortString cat)


categoryDecoder : D.Decoder Causality.Category
categoryDecoder =
    D.string |> D.map Causality.categoryFromShortString
