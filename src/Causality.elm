module Causality exposing (..)

import Graph
import IntDict
import Random
import Random.Float
import VegaLite as VL


type alias Contribution =
    Float


type alias Variable =
    { name : String
    , intercept : Int
    , noiseSd : Float
    }


type alias SortedVariables =
    List (Graph.NodeContext Int Contribution)


type alias SortedDAG =
    { variables : List Variable
    , sorted : SortedVariables
    }


type alias ProcessingContribution =
    { from : Int
    , to : Int
    , contribution : Contribution
    }


processSingleVar : ProcessingContribution -> IntDict.IntDict (List Float) -> IntDict.IntDict (List Float)
processSingleVar contrib oldDict =
    let
        oldTo =
            case IntDict.get contrib.to oldDict of
                Nothing ->
                    [ 0.0 ]

                Just x ->
                    x

        from =
            case IntDict.get contrib.from oldDict of
                Nothing ->
                    [ 0.0 ]

                Just x ->
                    x

        contribValue =
            List.map (\x -> x * contrib.contribution) from

        newTo =
            List.map2 (+) oldTo contribValue
    in
    IntDict.insert contrib.to newTo oldDict


singleNodeContrib : Graph.NodeContext Int Contribution -> List ProcessingContribution
singleNodeContrib context =
    let
        fromId =
            context.node.id

        edgeList =
            IntDict.toList context.outgoing

        singleEdgeToContrib =
            \( toId, contrib ) ->
                { from = fromId
                , to = toId
                , contribution = contrib
                }
    in
    List.map singleEdgeToContrib edgeList


allContrib : SortedVariables -> List ProcessingContribution
allContrib vars =
    List.map singleNodeContrib vars
        |> List.foldr (++) []


dataFromStandardNoise : SortedDAG -> List (List Float) -> VL.Data
dataFromStandardNoise dag stdNoise =
    let
        initFromVarAndNoise =
            \var singleNoise ->
                List.map (\x -> toFloat var.intercept + var.noiseSd * x) singleNoise

        init =
            List.map2 initFromVarAndNoise dag.variables stdNoise
                |> List.indexedMap Tuple.pair
                |> IntDict.fromList

        resultList =
            List.foldl processSingleVar init (allContrib dag.sorted)
                |> IntDict.values

        columnDefFromVarAndResult =
            \var singleRes ->
                VL.dataColumn var.name (VL.nums singleRes)

        allColumnDefs =
            List.map2 columnDefFromVarAndResult dag.variables resultList

        allColumns =
            List.foldl (<|) [] allColumnDefs
    in
    VL.dataFromColumns [] allColumns


noiseGenerator : Int -> Int -> Random.Generator (List (List Float))
noiseGenerator nVars nSubj =
    Random.list nVars
        (Random.list nSubj Random.Float.standardNormal)
