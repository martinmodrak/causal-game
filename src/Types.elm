module Types exposing (..)

import Json.Decode exposing (Error)


type alias Model =
    { imgData : Maybe String
    , webRReady : Bool
    , lastError : Maybe String
    }


type WebRMsg
    = WebRReady
    | ImageReceived String
    | InvalidWebR Error


type WebRCmd
    = InitWebR
    | GenerateImage


type Msg
    = WebR WebRMsg
