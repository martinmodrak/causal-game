module Causality exposing (..)

import Association exposing (Experiment)
import Graph
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import IntDict
import List exposing (range)
import Random
import Random.Float
import VegaLite as VL
import View exposing (vegaPlot)


type alias Contribution =
    Float


type CauseDirection
    = NoCause
    | Right
    | Left


type alias Experiment =
    { randomized : Bool
    , intervention : Int
    , n : Int
    }


type alias Variable =
    { name : String
    , intercept : Float
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


type alias Outcome =
    List (List Bool)


processSingleVar : Int -> ProcessingContribution -> IntDict.IntDict (List Float) -> IntDict.IntDict (List Float)
processSingleVar intervention contrib oldDict =
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

        contribMultiplier =
            if intervention == contrib.to then
                0.0

            else
                contrib.contribution

        contribValue =
            List.map
                (\x ->
                    contribMultiplier
                        * 0.5
                        * (if x > 0 then
                            1

                           else
                            -1
                          )
                )
                from

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


dataFromNoise : SortedDAG -> Int -> List (List Float) -> List (List Bool)
dataFromNoise dag intervention probitNoise =
    let
        singleInit =
            \id ( var, singleNoise ) ->
                let
                    interceptMult =
                        if id == intervention then
                            0.0

                        else
                            1.0
                in
                ( id, List.map (\x -> interceptMult * var.intercept + x) singleNoise )

        init =
            List.map2 Tuple.pair dag.variables probitNoise
                |> List.indexedMap singleInit
                |> IntDict.fromList

        resultsListFloat =
            List.foldl (processSingleVar intervention) init (allContrib dag.sorted)
                |> IntDict.values

        -- columnDefFromVarAndResult =
        --     \var singleRes ->
        --         VL.dataColumn var.name (VL.nums singleRes)
        -- allColumnDefs =
        --     List.map2 columnDefFromVarAndResult dag.variables resultList
        -- allColumns =
        --     List.foldl (<|) [] allColumnDefs
        -- VL.dataFromColumns [] allColumns
    in
    List.map (List.map (\x -> x > 0)) resultsListFloat


probitNoiseGenerator : Int -> Int -> Random.Generator (List (List Float))
probitNoiseGenerator nVars nSubj =
    Random.list nVars
        (Random.list nSubj Random.Float.standardNormal)


visNoiseGenerator : Int -> Int -> Random.Generator (List (List Float))
visNoiseGenerator nVars nSubj =
    Random.list nVars
        (Random.list nSubj (Random.float 0 1))


noIntervention : Int
noIntervention =
    -1


generatorObservational : SortedDAG -> Int -> Random.Generator Outcome
generatorObservational dag nSubj =
    probitNoiseGenerator (List.length dag.variables) nSubj
        |> Random.map (dataFromNoise dag noIntervention)


generatorRandomized : SortedDAG -> Int -> Int -> Random.Generator Outcome
generatorRandomized dag intervention nSubj =
    probitNoiseGenerator (List.length dag.variables) nSubj
        |> Random.map (dataFromNoise dag intervention)


singlePairSpec : String -> String -> ( VL.VLProperty, VL.Spec )
singlePairSpec xName yName =
    let
        jitExpr =
            \name ->
                "if(datum." ++ name ++ ", datum." ++ name ++ "Mean + (1 - datum." ++ name ++ "Mean) * random(), datum." ++ name ++ "Mean * random())"

        transforms =
            VL.transform
                << VL.joinAggregate
                    [ VL.opAs VL.opMean xName (xName ++ "Mean")
                    , VL.opAs VL.opMean yName (yName ++ "Mean")
                    ]
                    []
                << VL.calculateAs (jitExpr xName) (xName ++ "Jit")
                << VL.calculateAs (jitExpr yName)
                    (yName ++ "Jit")
                << VL.calculateAs "toString(datum.x) + toString(datum.y)"
                    (xName ++ "_" ++ yName)

        enc =
            VL.encoding
                << VL.position VL.X [ VL.pName (xName ++ "Jit"), VL.pQuant, VL.pAxis [ VL.axTitle xName ] ]
                << VL.position VL.Y [ VL.pName (yName ++ "Jit"), VL.pQuant, VL.pAxis [ VL.axTitle yName ] ]
                << VL.color [ VL.mName (xName ++ "_" ++ yName), VL.mNominal, VL.mLegend [] ]

        rule =
            \coord pos ->
                VL.asSpec
                    [ (VL.encoding << VL.position coord [ VL.pDatum (VL.num pos) ]) []
                    , VL.rule []
                    ]

        internalRule =
            \coord name ->
                VL.asSpec
                    [ (VL.encoding
                        << VL.position coord
                            [ VL.pName name, VL.pAggregate VL.opMean ]
                      )
                        []
                    , VL.rule []
                    ]
    in
    VL.layer
        [ VL.asSpec
            [ transforms []
            , enc []
            , VL.circle []
            ]
        , internalRule VL.X xName
        , internalRule VL.Y yName
        , rule VL.X 0
        , rule VL.X 1
        , rule VL.Y 0
        , rule VL.Y 1
        ]


outcomeToSpec : List String -> Outcome -> VL.Spec
outcomeToSpec varNames outcome =
    let
        columnDefFromVarAndResult =
            \var singleRes ->
                VL.dataColumn var (VL.boos singleRes)

        allColumnDefs =
            List.map2 columnDefFromVarAndResult varNames outcome

        allColumns =
            List.foldl (<|) [] allColumnDefs

        data =
            VL.dataFromColumns [] allColumns

        config =
            VL.configure
                << VL.configuration (VL.coAxis [ VL.axcoDomain False, VL.axcoTicks False, VL.axcoLabels False, VL.axcoGrid False ])

        plotRow =
            \xNames yName ->
                VL.vConcat
                    (List.map (\x -> singlePairSpec x yName) xNames
                        |> List.map (\x -> VL.asSpec [ x ])
                    )

        specSet =
            case varNames of
                [] ->
                    []

                _ :: [] ->
                    []

                a :: b :: [] ->
                    [ singlePairSpec a b ]

                _ ->
                    [ VL.hConcat
                        (List.map (plotRow varNames) varNames
                            |> List.map (\x -> VL.asSpec [ x ])
                        )
                    ]
    in
    VL.toVegaLite
        (data
            :: config []
            :: specSet
        )


causalityChooser : (CauseDirection -> msg) -> CauseDirection -> Html msg
causalityChooser causeMsg currentVal =
    let
        singleOption =
            \val label ->
                option [ Attr.selected (currentVal == val), Events.onClick (causeMsg val) ] [ text label ]
    in
    select []
        [ singleOption NoCause "is not causally related"
        , singleOption Right "causes"
        , singleOption Left "is caused by"
        ]


causalityProposedGuess : String -> String -> (CauseDirection -> msg) -> CauseDirection -> Html msg
causalityProposedGuess name1 name2 causeMsg currentVal =
    div []
        [ em [] [ text name1 ]
        , text " "
        , causalityChooser causeMsg currentVal
        , text " "
        , em [] [ text name2 ]
        ]


causalityDirectionToString : CauseDirection -> String
causalityDirectionToString dir =
    case dir of
        NoCause ->
            "is not causally related"

        Right ->
            "causes"

        Left ->
            "is caused by"


causalityDescription : String -> String -> CauseDirection -> Html msg
causalityDescription name1 name2 val =
    div []
        [ em [] [ text name1 ]
        , text " "
        , strong [] [ text (causalityDirectionToString val) ]
        , text " "
        , em [] [ text name2 ]
        ]


causeGenerator : Random.Generator CauseDirection
causeGenerator =
    Random.int 0 2
        |> Random.map
            (\x ->
                case x of
                    1 ->
                        Left

                    2 ->
                        Right

                    _ ->
                        NoCause
            )


contribGenerator : Random.Generator Float
contribGenerator =
    let
        fromSignAndVal =
            \sign val ->
                if sign then
                    val

                else
                    -val
    in
    Random.map2 fromSignAndVal (Random.uniform True [ False ]) (Random.float 0.5 1)
