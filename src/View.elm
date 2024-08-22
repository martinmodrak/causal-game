module View exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
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
            div [] [ text "No scenarios yet" ]


scenario : List (Html msg) -> List (Html msg) -> Html msg
scenario infoElements dataElements =
    div [ Attr.class "scenario" ]
        [ div [ Attr.class "info" ] infoElements
        , div [ Attr.class "data" ] dataElements
        ]


nChooser : (String -> msg) -> Int -> Html msg
nChooser setNMsg currentN =
    input [ Attr.type_ "text", Attr.value (String.fromInt currentN), Events.onInput setNMsg ] []


vegaPlot : VegaLite.Spec -> Html msg
vegaPlot spec =
    node "vega-plot" [ Attr.property "spec" spec ] []
