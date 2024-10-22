module View exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed as Keyed
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
    input [ Attr.type_ "text", Attr.value (String.fromInt currentN), Events.onInput setNMsg ] []


vegaPlot : VegaLite.Spec -> Html msg
vegaPlot spec =
    node "vega-plot" [ Attr.property "spec" spec ] []


experimentTitle : Int -> Html msg
experimentTitle id =
    h4 [] [ text ("Experiment " ++ String.fromInt (id + 1)) ]
