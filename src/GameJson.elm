module GameJson exposing (..)

import Association
import Causality
import Game
import Graph
import IntDict
import Json.Decode as D
import Json.Encode as E
import Random
import SingleRelationship
import ThreeWay
import TwoRelationships


type alias OutcomeGenerator spec experiment =
    spec -> experiment -> Random.Generator Causality.Outcome


associationScenarioEncoder :
    Game.Scenario Association.Spec Association.Experiment Association.Outcome Association.Guess
    -> E.Value
associationScenarioEncoder =
    scenarioEncoder associationSpecEncoder E.int categoryEncoder


associationInstanceEncoder : Game.Instance Association.Spec Association.Experiment Association.Outcome Association.Guess -> E.Value
associationInstanceEncoder =
    instanceEncoder associationSpecEncoder E.int categoryEncoder


associationScenarioDecoder : D.Decoder (Game.Scenario Association.Spec Association.Experiment Association.Outcome Association.Guess)
associationScenarioDecoder =
    scenarioDecoder Association.generator associationSpecDecoder D.int categoryDecoder


singleRelScenarioEncoder :
    Game.Scenario SingleRelationship.Spec SingleRelationship.Experiment SingleRelationship.Outcome SingleRelationship.Guess
    -> E.Value
singleRelScenarioEncoder =
    scenarioEncoder singleRelSpecEncoder experimentEncoder categoryEncoder


singleRelInstanceEncoder : Game.Instance SingleRelationship.Spec SingleRelationship.Experiment SingleRelationship.Outcome SingleRelationship.Guess -> E.Value
singleRelInstanceEncoder =
    instanceEncoder singleRelSpecEncoder experimentEncoder categoryEncoder


singleRelScenarioDecoder : D.Decoder (Game.Scenario SingleRelationship.Spec SingleRelationship.Experiment SingleRelationship.Outcome SingleRelationship.Guess)
singleRelScenarioDecoder =
    scenarioDecoder SingleRelationship.generator singleRelSpecDecoder experimentDecoder categoryDecoder


twoRelScenarioEncoder :
    Game.Scenario TwoRelationships.Spec TwoRelationships.Experiment TwoRelationships.Outcome TwoRelationships.Guess
    -> E.Value
twoRelScenarioEncoder =
    scenarioEncoder twoRelSpecEncoder experimentEncoder twoRelGuessEncoder


twoRelInstanceEncoder :
    Game.Instance TwoRelationships.Spec TwoRelationships.Experiment TwoRelationships.Outcome TwoRelationships.Guess
    -> E.Value
twoRelInstanceEncoder =
    instanceEncoder twoRelSpecEncoder experimentEncoder twoRelGuessEncoder


twoRelScenarioDecoder : D.Decoder (Game.Scenario TwoRelationships.Spec TwoRelationships.Experiment TwoRelationships.Outcome TwoRelationships.Guess)
twoRelScenarioDecoder =
    scenarioDecoder TwoRelationships.generator twoRelSpecDecoder experimentDecoder twoRelGuessDecoder


threeWayScenarioEncoder :
    Game.Scenario ThreeWay.Spec ThreeWay.Experiment ThreeWay.Outcome ThreeWay.Guess
    -> E.Value
threeWayScenarioEncoder =
    scenarioEncoder threeWaySpecEncoder experimentEncoder threeWayGuessEncoder


threeWayInstanceEncoder :
    Game.Instance ThreeWay.Spec ThreeWay.Experiment ThreeWay.Outcome ThreeWay.Guess
    -> E.Value
threeWayInstanceEncoder =
    instanceEncoder threeWaySpecEncoder experimentEncoder threeWayGuessEncoder


threeWayScenarioDecoder : D.Decoder (Game.Scenario ThreeWay.Spec ThreeWay.Experiment ThreeWay.Outcome ThreeWay.Guess)
threeWayScenarioDecoder =
    scenarioDecoder ThreeWay.generator threeWaySpecDecoder experimentDecoder threeWayGuessDecoder


scenarioEncoder :
    (spec -> E.Value)
    -> (experiment -> E.Value)
    -> (guess -> E.Value)
    -> Game.Scenario spec experiment Causality.Outcome guess
    -> E.Value
scenarioEncoder specEnc experimentEnc guessEnc scenario =
    -- keep only 40 or so experiments at most to avoid decoding problems
    E.object
        [ ( "history", E.list (instanceEncoder specEnc experimentEnc guessEnc) (List.take 40 scenario.history) )
        , ( "proposedExperiment", experimentEnc scenario.proposedExperiment )
        , ( "proposedGuess", guessEnc scenario.proposedGuess )
        , ( "seed", E.int scenario.seed )
        ]


scenarioDecoder :
    OutcomeGenerator spec experiment
    -> D.Decoder spec
    -> D.Decoder experiment
    -> D.Decoder guess
    -> D.Decoder (Game.Scenario spec experiment Causality.Outcome guess)
scenarioDecoder generator specDec experimentDec guessDec =
    D.map5 Game.Scenario
        (D.field "history" (D.list (instanceDecoder generator specDec experimentDec guessDec)))
        (D.field "proposedExperiment" experimentDec)
        (D.field "proposedGuess" guessDec)
        (D.succeed False)
        (D.field "seed" D.int)


instanceEncoder :
    (spec -> E.Value)
    -> (experiment -> E.Value)
    -> (guess -> E.Value)
    -> Game.Instance spec experiment Causality.Outcome guess
    -> E.Value
instanceEncoder specEnc experimentEnc guessEnc instance =
    let
        dataExp =
            List.map .experiment instance.data

        dataSeed =
            List.map .seed instance.data
    in
    E.object
        [ ( "spec", specEnc instance.spec )
        , ( "creatureName", E.string instance.creatureName )
        , ( "dataExp", E.list experimentEnc dataExp )
        , ( "dataSeed", E.list E.int dataSeed )
        , ( "guess"
          , case instance.guess of
                Just g ->
                    guessEnc g

                Nothing ->
                    E.null
          )
        ]


instanceDecoder :
    OutcomeGenerator spec experiment
    -> D.Decoder spec
    -> D.Decoder experiment
    -> D.Decoder guess
    -> D.Decoder (Game.Instance spec experiment Causality.Outcome guess)
instanceDecoder generator specDec experimentDec guessDec =
    D.map5 (instanceBuilder generator)
        (D.field "spec" specDec)
        (D.field "creatureName" D.string)
        (D.field "dataExp" (D.list experimentDec))
        (D.field "dataSeed" (D.list D.int))
        (D.field "guess" (D.nullable guessDec))


instanceBuilder : OutcomeGenerator spec experiment -> spec -> String -> List experiment -> List Int -> Maybe guess -> Game.Instance spec experiment Causality.Outcome guess
instanceBuilder generator spec creatureName expList seedList guess =
    { spec = spec
    , creatureName = creatureName
    , data = List.map2 (experimentWithOutcomeBuilder (generator spec)) expList seedList
    , guess = guess
    }


experimentWithOutcomeBuilder : (experiment -> Random.Generator Causality.Outcome) -> experiment -> Int -> Game.ExperimentWithOutcome experiment Causality.Outcome
experimentWithOutcomeBuilder generator experiment seed =
    let
        ( outcome, _ ) =
            Random.step (generator experiment) (Game.loadSeed seed)
    in
    { experiment = experiment
    , outcome = outcome
    , seed = seed
    }


outcomeEncoder : Causality.Outcome -> E.Value
outcomeEncoder outcome =
    E.list (E.list E.float) outcome


outcomeDecoder : D.Decoder Causality.Outcome
outcomeDecoder =
    D.list (D.list D.float)


rleEncode : List Bool -> List Int
rleEncode =
    rleEncodeInternal True 0


rleEncodeInternal : Bool -> Int -> List Bool -> List Int
rleEncodeInternal state count bools =
    case bools of
        head :: rest ->
            if head == state then
                rleEncodeInternal state (count + 1) rest

            else
                count :: rleEncodeInternal (not state) 1 rest

        [] ->
            if count == 0 then
                []

            else
                [ count ]


rleDecode : List Int -> List Bool
rleDecode =
    rleDecodeInternal True


rleDecodeInternal : Bool -> List Int -> List Bool
rleDecodeInternal state ints =
    case ints of
        head :: rest ->
            let
                new =
                    List.repeat head state
            in
            new ++ rleDecodeInternal (not state) rest

        [] ->
            []


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


threeWaySpecEncoder : ThreeWay.Spec -> E.Value
threeWaySpecEncoder spec =
    E.object
        [ ( "sorted", sortedDAGEncoder spec.sorted )
        , ( "cause01", categoryEncoder spec.cause01 )
        , ( "cause02", categoryEncoder spec.cause02 )
        , ( "cause12", categoryEncoder spec.cause12 )
        ]


threeWaySpecDecoder : D.Decoder ThreeWay.Spec
threeWaySpecDecoder =
    D.map4 (\s c01 c02 c12 -> { sorted = s, cause01 = c01, cause02 = c02, cause12 = c12 })
        (D.field "sorted" sortedDagDecoder)
        (D.field "cause01" categoryDecoder)
        (D.field "cause02" categoryDecoder)
        (D.field "cause12" categoryDecoder)


threeWayGuessEncoder : ThreeWay.Guess -> E.Value
threeWayGuessEncoder guess =
    E.object
        [ ( "cause01", categoryEncoder guess.cause01 )
        , ( "cause02", categoryEncoder guess.cause02 )
        , ( "cause12", categoryEncoder guess.cause12 )
        ]


threeWayGuessDecoder : D.Decoder ThreeWay.Guess
threeWayGuessDecoder =
    D.map3 (\c01 c02 c12 -> { cause01 = c01, cause02 = c02, cause12 = c12 })
        (D.field "cause01" categoryDecoder)
        (D.field "cause02" categoryDecoder)
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
