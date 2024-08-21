module View exposing (view)

import Html exposing (Html, div, h2, img, input, node, text)
import Html.Attributes exposing (attribute, class, property, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Types exposing (..)
import VegaLite exposing (Data, Position(..), Spec, circle, color, dataFromUrl, encoding, mName, pName, pQuant, position, toVegaLite)


view : Model -> Html Msg
view model =
    div []
        [ Html.map AssocMsg (viewAssoc model.association) ]


viewAssoc : AssociationModel -> Html AssociationMsg
viewAssoc model =
    case model.scenarios of
        head :: rest ->
            div []
                (viewSingleAssocScenario True model.proposedExperiment head
                    :: List.map (viewSingleAssocScenario False model.proposedExperiment) rest
                )

        [] ->
            div [] [ text "No scenarios yet" ]


viewSingleAssocScenario : Bool -> AssociationExperiment -> AssociationScenario -> Html AssociationMsg
viewSingleAssocScenario isactive experiment scModel =
    div [ class "scenario" ]
        [ viewAssociationInfo isactive experiment scModel.spec
        , viewScenarioData viewSingleAssocScenarioData scModel.spec scModel.data
        ]


viewScenarioData : (specType -> data -> Html msg) -> specType -> List data -> Html msg
viewScenarioData singleView spec dataList =
    div [] (List.map (singleView spec) dataList)


viewSingleAssocScenarioData : AssociationSpec -> ( AssociationExperiment, VegaLite.Data ) -> Html AssociationMsg
viewSingleAssocScenarioData spec ( _, data ) =
    vegaPlot (associationVegaSpec spec data)


viewAssociationInfo : Bool -> AssociationExperiment -> AssociationSpec -> Html AssociationMsg
viewAssociationInfo isactive experiment _ =
    div []
        (h2 [] [ text "Is there an association?" ]
            :: (if isactive then
                    [ viewNChooser AssocSetN experiment
                    , input [ type_ "button", onClick AssocRunExperiment, value "Gather more data" ] []
                    ]

                else
                    []
               )
        )


viewNChooser : (String -> msg) -> Int -> Html msg
viewNChooser setNMsg currentN =
    input [ type_ "text", value (String.fromInt currentN), onInput setNMsg ] []


associationVegaSpec : AssociationSpec -> Data -> Spec
associationVegaSpec _ data =
    let
        enc =
            encoding
                << position X [ pName "x", pQuant ]
                << position Y [ pName "y", pQuant ]
    in
    toVegaLite [ data, enc [], circle [] ]


vegaPlot : VegaLite.Spec -> Html AssociationMsg
vegaPlot spec =
    node "vega-plot" [ property "spec" spec ] []
