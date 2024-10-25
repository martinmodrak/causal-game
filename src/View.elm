module View exposing (..)

import Constants
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as Keyed
import Json.Decode
import VegaLite


scenariosList : (Bool -> a -> Html msg) -> List a -> Html msg
scenariosList viewSingle scenarios =
    case scenarios of
        head :: rest ->
            div []
                (viewSingle True head
                    :: List.map (viewSingle False) rest
                )

        [] ->
            div [] [ text "N scenarios yet" ]


scenario : List (Html msg) -> List (Html msg) -> Html msg
scenario infoElements dataElements =
    let
        nElements =
            List.length dataElements

        keys =
            List.range 1 nElements |> List.map (\x -> String.fromInt (nElements - x))

        keyedElements =
            List.map2 Tuple.pair keys dataElements
    in
    div [ Attr.class "scenario" ]
        [ div [ Attr.class "info" ] infoElements
        , Keyed.node "div" [ Attr.class "data" ] keyedElements
        ]


nChooser : (String -> msg) -> Int -> Html msg
nChooser setNMsg currentN =
    let
        base =
            10

        mult =
            100

        logMsg =
            \x ->
                case String.toFloat x of
                    Just val ->
                        setNMsg (String.fromInt (round (base ^ (val / mult))))

                    Nothing ->
                        setNMsg (String.fromInt currentN)
    in
    span [ Attr.class "nChooser" ]
        [ input [ Attr.class "num", Attr.type_ "text", Attr.value (String.fromInt currentN), Events.onInput setNMsg ] []
        , input
            [ Attr.class "range"
            , Attr.type_ "range"
            , Attr.min "0"
            , Attr.max (String.fromInt (round (mult * logBase base (toFloat Constants.maxN))))
            , Attr.value (String.fromInt (round (mult * logBase base (toFloat currentN))))
            , Events.onInput logMsg
            ]
            []
        ]


vegaPlot : VegaLite.Spec -> Html msg
vegaPlot spec =
    node "vega-plot" [ Attr.property "spec" spec ] []


experimentTitle : Int -> Html msg
experimentTitle id =
    h4 [] [ text ("Experiment " ++ String.fromInt (id + 1)) ]


onChange : (String -> msg) -> Attribute msg
onChange messageCreator =
    Events.on "change" (Json.Decode.map messageCreator Events.targetValue)
