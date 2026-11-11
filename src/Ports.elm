port module Ports exposing (..)

import Json.Encode


port setStorage : Json.Encode.Value -> Cmd msg


port downloadFile : { name : String, mime : String, content : String } -> Cmd msg
