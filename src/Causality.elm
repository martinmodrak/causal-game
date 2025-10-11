module Causality exposing (..)

import Constants
import Game
import Graph
import Html exposing (..)
import Html.Attributes as Attr
import Html.Lazy
import IntDict
import Json.Encode
import List
import Random
import Random.Float
import Round
import Utils
import VegaLite as VL
import View


type alias Contribution =
    Float


type Category
    = NoCause
    | RightPos
    | RightNeg
    | LeftPos
    | LeftNeg


type alias Experiment =
    { randomized : Bool
    , intervention : Int
    , n : Int
    }


type ExpMsg
    = SetN String
    | SetRandomized Bool
    | SetIntervention Int


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
    List (List Float)


processSingleVar : Int -> ProcessingContribution -> IntDict.IntDict (List Float) -> IntDict.IntDict (List Float)
processSingleVar intervention contrib oldDict =
    let
        oldTo =
            case IntDict.get contrib.to oldDict of
                Nothing ->
                    [ 0 / 0 ]

                Just x ->
                    x

        from =
            case IntDict.get contrib.from oldDict of
                Nothing ->
                    [ 0 / 0 ]

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


dataFromNoise : SortedDAG -> Int -> List (List Float) -> Outcome
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
    in
    List.foldl (processSingleVar intervention) init (allContrib dag.sorted)
        |> IntDict.values


probitNoiseGenerator : Int -> Int -> Random.Generator (List (List Float))
probitNoiseGenerator nVars nSubj =
    Random.list nVars
        (Random.list nSubj Random.Float.standardNormal)


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


singlePairBeehiveDataSpec : List Bool -> List Float -> String -> String -> VL.Spec
singlePairBeehiveDataSpec xValues yValues xName yName =
    let
        data =
            (VL.dataFromColumns []
                << VL.dataColumn "x" (VL.boos xValues)
                << VL.dataColumn "y" (VL.nums yValues)
            )
                []

        encShared =
            VL.encoding
                << VL.position VL.X [ VL.pName "x", VL.pNominal, VL.pAxis [ VL.axTitle xName ] ]

        encPoints =
            VL.encoding
                << VL.color [ VL.mName "x", VL.mNominal, VL.mLegend [] ]
                << VL.shape [ VL.mName "x", VL.mNominal, VL.mLegend [] ]
                << VL.position VL.XOffset [ VL.pName "offset", VL.pQuant, VL.pScale [ VL.scDomain (VL.doNums [ 0, 1 ]) ] ]
                << VL.position VL.Y [ VL.pName "y", VL.pQuant, VL.pAxis [ VL.axTitle yName ] ]

        transPoints =
            VL.transform
                << VL.calculateAs "random()" "offset"

        config =
            VL.configure
                << VL.configuration
                    (VL.coAxis [ VL.axcoDomain True, VL.axcoTicks True, VL.axcoLabels True, VL.axcoLabelExpr "datum.value ? 'Yes' : 'No'", VL.axcoGrid False ] |> VL.coAxisXFilter)
                << VL.configuration
                    (VL.coAxis [ VL.axcoDomain False, VL.axcoTicks True, VL.axcoLabels False, VL.axcoGrid False ] |> VL.coAxisYFilter)

        pointLayer =
            VL.asSpec
                [ VL.width 150
                , encPoints []
                , transPoints []
                , VL.point [ VL.maFilled True, VL.maOpacity 0.7 ]
                ]

        encMean =
            VL.encoding
                << VL.position VL.Y [ VL.pName "y", VL.pAggregate VL.opMean, VL.pQuant, VL.pTitle "" ]

        meanLayer =
            VL.asSpec
                [ encMean []
                , VL.point [ VL.maFilled True, VL.maSize 150, VL.maColor "black" ]
                ]

        encErrorBar =
            VL.encoding
                << VL.position VL.Y [ VL.pName "y", VL.pQuant, VL.pTitle "" ]

        errorBarLayer =
            VL.asSpec
                [ encErrorBar []
                , VL.errorbar [ VL.maExtent VL.exCi, VL.maThickness 3 ]
                ]
    in
    VL.toVegaLite
        [ data
        , config []
        , encShared []
        , VL.layer
            [ pointLayer
            , meanLayer
            , errorBarLayer
            ]
        ]


viewSingleDiagonal : List Bool -> String -> Html a
viewSingleDiagonal _ name =
    -- let
    --     n =
    --         List.length vals
    --     true =
    --         List.map Utils.boolToInt vals |> List.sum
    --     mean =
    --         toFloat true / toFloat n
    -- in
    div []
        [ h4 [ Attr.class "diagonalTitle" ] [ text name ]

        -- , text (String.fromInt n)
        -- , text " ("
        -- , text (Round.round 1 (mean * 100))
        -- , text "%) true"
        ]


viewSingleContingency : List Bool -> List Bool -> String -> String -> Html a
viewSingleContingency xValues yValues xName yName =
    let
        ( _, counts ) =
            positionInGroupInner { ff = 0, ft = 0, tf = 0, tt = 0 } xValues yValues
    in
    table []
        [ tr []
            [ th [ Attr.rowspan 3, Attr.class "yName" ] [ text yName ]
            , th [] [ text "Yes" ]
            , td [] [ text (String.fromInt counts.ft) ]
            , td [] [ text (String.fromInt counts.tt) ]
            , td [ Attr.class "ratioCol" ] [ text (Round.round 2 (toFloat counts.ft / toFloat counts.tt)) ]
            ]
        , tr []
            [ th [] [ text "No" ]
            , td [] [ text (String.fromInt counts.ff) ]
            , td [] [ text (String.fromInt counts.tf) ]
            , td [ Attr.class "ratioCol" ] [ text (Round.round 2 (toFloat counts.ff / toFloat counts.tf)) ]
            ]
        , tr [ Attr.class "ratioRow" ]
            [ th [] [ text "Ratio Y/N" ]
            , td [] [ text (Round.round 2 (toFloat counts.ft / toFloat counts.ff)) ]
            , td [] [ text (Round.round 2 (toFloat counts.tt / toFloat counts.tf)) ]
            , td [ Attr.class "ratioCol" ] []
            ]
        , tr []
            [ td [] []
            , td [] []
            , th [] [ text "No" ]
            , th [] [ text "Yes" ]
            , th [ Attr.class "ratioCol" ] [ text "Ratio N/Y" ]
            ]
        , tr []
            [ td [] []
            , td [] []
            , th
                [ Attr.colspan 3, Attr.class "xName" ]
                [ text xName ]
            ]
        ]


viewSingleBeehive : List Bool -> List Float -> String -> String -> Html Never
viewSingleBeehive xValues yValues xName yName =
    View.vegaPlot (singlePairBeehiveDataSpec xValues yValues xName yName)


viewOutcomeSubplot : Int -> Int -> List Float -> List Float -> String -> String -> Html Never
viewOutcomeSubplot xOrd yOrd xValues yValues xName yName =
    let
        floatToBin =
            List.map (\x -> x > 0)
    in
    if xOrd > 0 && yOrd == 0 then
        td [ Attr.class "beehive" ] [ viewSingleBeehive (floatToBin xValues) yValues xName yName ]

    else if xOrd > yOrd then
        td [ Attr.class "contigency" ] [ viewSingleContingency (floatToBin xValues) (floatToBin yValues) xName yName ]

    else
        td [] []


viewOutcome : Game.ViewSettings -> SortedDAG -> Outcome -> Html Never
viewOutcome _ sorted outcome =
    let
        varNames =
            variableNames sorted

        plotRow =
            \yOrd ( yName, yVals ) ->
                tr []
                    (List.indexedMap
                        (\ord ( name, vals ) ->
                            viewOutcomeSubplot ord yOrd vals yVals name yName
                        )
                        (List.map2 Tuple.pair varNames outcome)
                    )

        allRows =
            case varNames of
                [] ->
                    []

                _ :: [] ->
                    []

                -- a :: b :: [] ->
                --     [ singlePairWaffleDataSpec a b ]
                _ ->
                    -- _ :: _ :: _ :: _ ->
                    List.indexedMap plotRow (List.map2 Tuple.pair varNames outcome)
    in
    table [] allRows


viewProposedExperiment : SortedDAG -> Experiment -> Html ExpMsg
viewProposedExperiment sorted experiment =
    let
        randomizedOption =
            \val label ->
                option [ Attr.selected (experiment.randomized == val), Attr.value (Utils.boolToString val) ] [ text label ]

        interventionOption =
            \id var ->
                option [ Attr.selected (experiment.intervention == id), Attr.value (String.fromInt id) ] [ text var.name ]

        intervention =
            if experiment.randomized then
                span []
                    [ text ", randomizing "
                    , select [ View.onChange (String.toInt >> Maybe.withDefault noIntervention >> SetIntervention) ]
                        (List.indexedMap interventionOption sorted.variables)
                    ]

            else
                text ""
    in
    div []
        [ text "Run an "
        , select [ View.onChange (Utils.boolFromString >> SetRandomized) ]
            [ randomizedOption False "observational"
            , randomizedOption True "randomized"
            ]
        , text " study "
        , intervention
        , text " with "
        , View.nChooser SetN experiment.n
        , text " participants."
        ]


causalityChooser : (Category -> msg) -> Category -> Html msg
causalityChooser causeMsg currentVal =
    let
        singleOption =
            \val ->
                option [ Attr.selected (currentVal == val), Attr.value (categoryToShortString val) ] [ text (categoryToString val) ]
    in
    select [ View.onChange (categoryFromShortString >> causeMsg) ]
        [ singleOption NoCause
        , singleOption RightPos
        , singleOption RightNeg
        , singleOption LeftPos
        , singleOption LeftNeg
        ]


causalityProposedGuess : String -> String -> (Category -> msg) -> Category -> Html msg
causalityProposedGuess name1 name2 causeMsg currentVal =
    div []
        [ em [] [ text name1 ]
        , text " "
        , causalityChooser causeMsg currentVal
        , text " "
        , em [] [ text name2 ]
        ]


categoryToString : Category -> String
categoryToString dir =
    case dir of
        NoCause ->
            "is not causally related to"

        RightPos ->
            "promotes"

        RightNeg ->
            "inhibits"

        LeftPos ->
            "is promoted by"

        LeftNeg ->
            "is inhibited by"


categoryToShortString : Category -> String
categoryToShortString cat =
    case cat of
        NoCause ->
            "NoCause"

        RightPos ->
            "RightPos"

        RightNeg ->
            "RightNeg"

        LeftPos ->
            "LeftPos"

        LeftNeg ->
            "LeftNeg"


categoryFromShortString : String -> Category
categoryFromShortString cat =
    case cat of
        "NoCause" ->
            NoCause

        "RightPos" ->
            RightPos

        "RightNeg" ->
            RightNeg

        "LeftPos" ->
            LeftPos

        "LeftNeg" ->
            LeftNeg

        _ ->
            NoCause


categoryMult : Category -> Float
categoryMult cat =
    case cat of
        RightNeg ->
            -1

        LeftNeg ->
            -1

        NoCause ->
            0

        _ ->
            1


causalityDescription : String -> String -> Category -> Html msg
causalityDescription name1 name2 val =
    div []
        [ em [] [ text name1 ]
        , text " "
        , strong [] [ text (categoryToString val) ]
        , text " "
        , em [] [ text name2 ]
        ]


categoryGenerator : Random.Generator Category
categoryGenerator =
    Random.weighted ( 2, NoCause ) [ ( 1, RightNeg ), ( 1, RightPos ), ( 1, LeftNeg ), ( 1, LeftPos ) ]


interceptGenerator : Random.Generator Float
interceptGenerator =
    Random.float -1 1


contribGenerator : Category -> Random.Generator Float
contribGenerator cat =
    let
        fromVal =
            \val ->
                val * categoryMult cat
    in
    Random.map fromVal (Random.float 1 2.5)


outcomeGenerator : SortedDAG -> Experiment -> Random.Generator Outcome
outcomeGenerator sorted experiment =
    if experiment.randomized then
        generatorRandomized sorted experiment.intervention experiment.n

    else
        generatorObservational sorted experiment.n


variableNames : SortedDAG -> List String
variableNames sorted =
    sorted.variables |> List.map .name


updateExperiment : ExpMsg -> Experiment -> Experiment
updateExperiment msg experiment =
    case msg of
        SetN newN ->
            case String.toInt newN of
                Just n ->
                    { experiment | n = min n Constants.maxN }

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


viewExperiment : Game.ViewSettings -> SortedDAG -> Int -> ( Experiment, Outcome ) -> Html Never
viewExperiment viewSettings sorted id ( experiment, data ) =
    let
        typeText =
            if experiment.randomized then
                "Randomizing "
                    -- TODO mess
                    ++ (case List.indexedMap Tuple.pair sorted.variables |> List.filter (\( varId, _ ) -> varId == experiment.intervention) of
                            [] ->
                                "Error"

                            ( _, var ) :: _ ->
                                var.name
                       )

            else
                "Observational study"
    in
    div [ Attr.class "experiment" ]
        [ View.experimentTitle id
        , p []
            [ strong [] [ text typeText ]
            , br [] []
            , text ("N = " ++ String.fromInt experiment.n ++ ", CZK " ++ String.fromInt (costExperiment experiment))
            ]
        , Html.Lazy.lazy3 viewOutcome viewSettings sorted data
        ]


edgeListFromCause : Int -> Int -> Category -> Float -> List (Graph.Edge Float)
edgeListFromCause =
    \id1 id2 cause contrib ->
        case cause of
            NoCause ->
                []

            RightPos ->
                [ { from = id1, to = id2, label = contrib } ]

            RightNeg ->
                [ { from = id1, to = id2, label = contrib } ]

            LeftPos ->
                [ { from = id2, to = id1, label = contrib } ]

            LeftNeg ->
                [ { from = id2, to = id1, label = contrib } ]
