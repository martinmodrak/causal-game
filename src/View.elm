module View exposing (view)

import Html exposing (Html, div, h2, img, input, node, text)
import Html.Attributes exposing (attribute, class, property, src, type_, value)
import Html.Events exposing (onClick)
import Types exposing (AssociationSpec, Model, Msg(..), Scenario(..), ScenarioModel)
import VegaLite exposing (Data, Position(..), Spec, circle, color, dataFromUrl, encoding, mName, pName, pQuant, position, toVegaLite)


view : Model -> Html Msg
view model =
    div []
        (List.map viewSingleScenario model.scenarios)


viewSingleScenario : ScenarioModel -> Html Msg
viewSingleScenario scModel =
    div [ class "scenario" ]
        [ viewScenarioInfo scModel.scenario
        , viewScenarioData scModel
        ]


viewScenarioData : ScenarioModel -> Html Msg
viewScenarioData scModel =
    div [] (List.map (viewSingleScenarioData scModel.scenario) scModel.data)


viewSingleScenarioData : Scenario -> VegaLite.Data -> Html Msg
viewSingleScenarioData scenario data =
    let
        plotSpec =
            case scenario of
                Association assocSpec ->
                    associationVegaSpec assocSpec data
    in
    vegaPlot plotSpec


viewScenarioInfo : Scenario -> Html Msg
viewScenarioInfo scenario =
    div []
        [ h2 [] [ text (scenarioTitle scenario) ]
        , input [ type_ "button", onClick (GetData scenario), value "Gather more data" ] []
        ]


scenarioTitle : Scenario -> String
scenarioTitle scenario =
    case scenario of
        Association _ ->
            "Is there an association?"


associationVegaSpec : AssociationSpec -> Data -> Spec
associationVegaSpec _ data =
    let
        enc =
            encoding
                << position X [ pName "x", pQuant ]
                << position Y [ pName "y", pQuant ]
    in
    toVegaLite [ data, enc [], circle [] ]


vegaPlot : VegaLite.Spec -> Html Msg
vegaPlot spec =
    node "vega-plot" [ property "spec" spec ] []
