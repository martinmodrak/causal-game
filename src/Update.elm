module Update exposing (update)

import Json.Decode
import Types exposing (Model, Msg(..), WebRCmd(..), WebRMsg(..))
import WebR exposing (sendWebR)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WebR webRmsg ->
            updateWebR webRmsg model


updateWebR : WebRMsg -> Model -> ( Model, Cmd Msg )
updateWebR msg model =
    case msg of
        WebRReady ->
            ( { model | webRReady = True }, sendWebR GenerateImage )

        ImageReceived imageData ->
            ( { model | imgData = Just imageData }, Cmd.none )

        InvalidWebR err ->
            ( { model | lastError = Just (Json.Decode.errorToString err) }, Cmd.none )
