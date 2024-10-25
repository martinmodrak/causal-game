module Causality exposing (..)

import Constants
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
    List (List Bool)


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


positionInGroupInner : { ff : Int, ft : Int, tf : Int, tt : Int } -> List Bool -> List Bool -> ( List ( Float, Float ), { ff : Int, ft : Int, tf : Int, tt : Int } )
positionInGroupInner countsSoFar xValues yValues =
    case ( xValues, yValues ) of
        ( valX :: xRest, valY :: yRest ) ->
            let
                ( accessor, recurseWith ) =
                    if valX then
                        if valY then
                            ( .tt, { countsSoFar | tt = countsSoFar.tt + 1 } )

                        else
                            ( .tf, { countsSoFar | tf = countsSoFar.tf + 1 } )

                    else if valY then
                        ( .ft, { countsSoFar | ft = countsSoFar.ft + 1 } )

                    else
                        ( .ff, { countsSoFar | ff = countsSoFar.ff + 1 } )

                ( posRest, totalCounts ) =
                    positionInGroupInner recurseWith xRest yRest

                totalCount =
                    accessor totalCounts

                currentOrd =
                    accessor countsSoFar

                nData =
                    toFloat (totalCounts.tt + totalCounts.tf + totalCounts.ft + totalCounts.ff)

                rectWidth =
                    toFloat
                        (if valX then
                            totalCounts.tt + totalCounts.tf

                         else
                            totalCounts.ft + totalCounts.ff
                        )
                        / nData

                rectHeight =
                    toFloat
                        (if valY then
                            totalCounts.tt + totalCounts.ft

                         else
                            totalCounts.tf + totalCounts.ff
                        )
                        / nData

                sqrtAreaPerPoint =
                    sqrt (rectWidth * rectHeight / toFloat totalCount)

                minPerRowX =
                    ceiling (rectWidth / sqrtAreaPerPoint)

                minPerRowY =
                    ceiling (rectHeight / sqrtAreaPerPoint)

                stepSize =
                    min (rectWidth / toFloat minPerRowX) (rectHeight / toFloat minPerRowY)

                remainderX =
                    max 0 (rectWidth - stepSize * (1 + toFloat ((totalCount - 1) // minPerRowY)))

                maxFilledY =
                    if totalCount >= minPerRowY then
                        minPerRowY

                    else
                        totalCount

                remainderY =
                    max 0 (rectHeight - stepSize * toFloat maxFilledY)

                xPos =
                    remainderX * 0.5 + stepSize * (0.5 + toFloat (currentOrd // minPerRowY))

                yPos =
                    remainderY * 0.5 + stepSize * (0.5 + toFloat (modBy minPerRowY currentOrd))
            in
            ( ( xPos, yPos ) :: posRest, totalCounts )

        _ ->
            ( [], countsSoFar )


positionInGroup : List Bool -> List Bool -> List ( Float, Float )
positionInGroup xValues yValues =
    let
        ( res, _ ) =
            positionInGroupInner { ff = 0, ft = 0, tf = 0, tt = 0 } xValues yValues
    in
    res


singlePairWaffleDataSpec : List Bool -> List Bool -> String -> String -> VL.Spec
singlePairWaffleDataSpec xValues yValues xName yName =
    let
        ( xPos, yPos ) =
            positionInGroup xValues yValues
                |> List.unzip

        data =
            (VL.dataFromColumns []
                << VL.dataColumn "x" (VL.boos xValues)
                << VL.dataColumn "y" (VL.boos yValues)
                << VL.dataColumn "xPos" (VL.nums xPos)
                << VL.dataColumn "yPos" (VL.nums yPos)
            )
                []

        jitExpr =
            \name ->
                "if(datum." ++ name ++ ", (1 - datum." ++ name ++ "Mean) + datum." ++ name ++ "Pos,  datum." ++ name ++ "Pos)"

        transforms =
            VL.transform
                << VL.joinAggregate
                    [ VL.opAs VL.opMean "x" "xMean"
                    , VL.opAs VL.opMean "y" "yMean"
                    ]
                    []
                << VL.calculateAs (jitExpr "x") "xJit"
                << VL.calculateAs (jitExpr "y")
                    "yJit"
                << VL.calculateAs "toString(datum.x) + toString(datum.y)"
                    "x_y"

        enc =
            VL.encoding
                << VL.position VL.X [ VL.pName "xJit", VL.pQuant, VL.pAxis [ VL.axTitle xName ] ]
                << VL.position VL.Y [ VL.pName "yJit", VL.pQuant, VL.pAxis [ VL.axTitle yName ] ]
                << VL.color [ VL.mName "x_y", VL.mNominal, VL.mLegend [] ]

        rule =
            \coord pos ->
                VL.asSpec
                    [ (VL.encoding << VL.position coord [ VL.pDatum (VL.num pos) ]) []
                    , VL.rule []
                    ]

        internalRule =
            \coord name ->
                VL.asSpec
                    [ (VL.transform
                        << VL.calculateAs ("1 - datum." ++ name) (name ++ "1m")
                      )
                        []
                    , (VL.encoding
                        << VL.position coord
                            [ VL.pName (name ++ "1m")
                            , VL.pAggregate VL.opMean
                            ]
                      )
                        []
                    , VL.rule []
                    ]

        config =
            VL.configure
                << VL.configuration (VL.coAxis [ VL.axcoDomain False, VL.axcoTicks False, VL.axcoLabels False, VL.axcoGrid False ])

        myAxLabel =
            \val isX ->
                let
                    ( text, align ) =
                        if val then
                            ( "True", "right" )

                        else
                            ( "False", "left" )

                    ( angle, dy, baseline ) =
                        if isX then
                            ( 0, 5, "top" )

                        else
                            ( 270, -5, "bottom" )

                    ( x, y ) =
                        if isX then
                            ( if val then
                                Json.Encode.string "width"

                              else
                                Json.Encode.float 0
                            , Json.Encode.string "height"
                            )

                        else
                            ( Json.Encode.float 0
                            , if val then
                                Json.Encode.float 0

                              else
                                Json.Encode.string "height"
                            )
                in
                Json.Encode.object
                    [ ( "mark"
                      , Json.Encode.object
                            [ ( "type", Json.Encode.string "text" )
                            , ( "text", Json.Encode.string text )
                            , ( "x", x )
                            , ( "y", y )
                            , ( "align", Json.Encode.string align )
                            , ( "baseline", Json.Encode.string baseline )
                            , ( "dy", Json.Encode.float dy )
                            , ( "angle", Json.Encode.float angle )
                            ]
                      )
                    ]

        spec =
            VL.layer
                [ VL.asSpec
                    [ transforms []
                    , enc []
                    , VL.circle []
                    ]
                , internalRule VL.X "x"
                , internalRule VL.Y "y"
                , rule VL.X 0
                , rule VL.X 1
                , rule VL.Y 0
                , rule VL.Y 1
                , myAxLabel False False
                , myAxLabel False True
                , myAxLabel True False
                , myAxLabel True True
                ]
    in
    VL.toVegaLite
        [ data
        , config []
        , spec
        ]


viewSingleDiagonal : List Bool -> String -> Html a
viewSingleDiagonal vals name =
    let
        n =
            List.length vals

        true =
            List.map Utils.boolToInt vals |> List.sum

        mean =
            toFloat true / toFloat n
    in
    div []
        [ h4 [ Attr.class "diagonalTitle" ] [ text name ]
        , text (String.fromInt n)
        , text " ("
        , text (Round.round 1 (mean * 100))
        , text "%) true"
        ]


viewSingleContingency : List Bool -> List Bool -> String -> String -> Html a
viewSingleContingency xValues yValues xName yName =
    let
        ( _, counts ) =
            positionInGroupInner { ff = 0, ft = 0, tf = 0, tt = 0 } xValues yValues
    in
    table []
        [ tr []
            [ th [ Attr.rowspan 2 ] [ text yName ]
            , th [] [ text "True" ]
            , td [] [ text (String.fromInt counts.tf) ]
            , td [] [ text (String.fromInt counts.tt) ]
            ]
        , tr []
            [ th [] [ text "False" ]
            , td [] [ text (String.fromInt counts.ff) ]
            , td [] [ text (String.fromInt counts.ft) ]
            ]
        , tr []
            [ td [] []
            , td [] []
            , th [] [ text "False" ]
            , th [] [ text "True" ]
            ]
        , tr []
            [ td [] []
            , td [] []
            , th
                [ Attr.colspan 2 ]
                [ text xName ]
            ]
        ]


viewOutcomeSubplot : Int -> Int -> List Bool -> List Bool -> String -> String -> Html Never
viewOutcomeSubplot xOrd yOrd xValues yValues xName yName =
    if xOrd < yOrd then
        td [ Attr.class "waffle" ] [ View.vegaPlot (singlePairWaffleDataSpec xValues yValues xName yName) ]

    else
        td [] []



-- else if xOrd == yOrd then
--     td [ Attr.class "diagonal" ] [ viewSingleDiagonal xValues xName ]
-- else
--     td [ Attr.class "contigency" ] [ viewSingleContingency xValues yValues xName yName ]


viewOutcome : SortedDAG -> Outcome -> Html Never
viewOutcome sorted outcome =
    let
        varNames =
            variableNames sorted

        plotRow =
            \yOrd ( yName, yVals ) ->
                tr []
                    (List.indexedMap (\ord ( name, vals ) -> viewOutcomeSubplot ord yOrd vals yVals name yName) (List.map2 Tuple.pair varNames outcome))

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


viewExperiment : SortedDAG -> Int -> ( Experiment, Outcome ) -> Html Never
viewExperiment sorted id ( experiment, data ) =
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
        , Html.Lazy.lazy2 viewOutcome sorted data
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
