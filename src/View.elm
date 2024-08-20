module View exposing (view)

import Html exposing (Html, img, node, text)
import Html.Attributes exposing (attribute, property, src)
import Types exposing (Model, Msg)
import VegaLite exposing (Position(..), Spec, circle, color, dataFromUrl, encoding, mName, pName, pQuant, position, toVegaLite)


view : Model -> Html Msg
view model =
    node "vega-plot" [ property "spec" testSpec ] []


testSpec : Spec
testSpec =
    let
        path =
            "https://cdn.jsdelivr.net/npm/vega-datasets@2/data/"

        data =
            dataFromUrl (path ++ "penguins.json") []

        enc =
            encoding
                << position X [ pName "Beak Length (mm)", pQuant ]
                << position Y [ pName "Body Mass (g)", pQuant ]
                << color [ mName "Species" ]
    in
    toVegaLite [ data, enc [], circle [] ]
