module View exposing (view)

import Html exposing (Html, img, text)
import Html.Attributes exposing (src)
import Types exposing (Model, Msg)


dataUrlFromImage : String -> String
dataUrlFromImage pngString =
    "data:image/png;base64," ++ pngString


view : Model -> Html Msg
view model =
    if not model.webRReady then
        text "Initializing webR"

    else
        case model.imgData of
            Just dataString ->
                img [ src (dataUrlFromImage dataString) ] []

            Nothing ->
                text "Waiting for image"
