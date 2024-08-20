port module WebR exposing (..)

import Json.Decode as D
import Json.Encode as E
import Types exposing (Msg(..), WebRCmd(..), WebRMsg(..))


port sendPort : E.Value -> Cmd msg


port receivePort : (D.Value -> msg) -> Sub msg


receiveSub : Sub Msg
receiveSub =
    receivePort receiveWebR


receiveWebR : D.Value -> Msg
receiveWebR json =
    case D.decodeValue receiveDecode json of
        Ok msg ->
            WebR msg

        Err err ->
            WebR (InvalidWebR err)


receiveDecode : D.Decoder WebRMsg
receiveDecode =
    D.field "msg" D.string
        |> D.andThen receiveDecodeByTag


receiveDecodeByTag : String -> D.Decoder WebRMsg
receiveDecodeByTag tag =
    case tag of
        "ready" ->
            D.succeed WebRReady

        "image" ->
            D.map ImageReceived (D.field "img" D.string)

        _ ->
            D.fail ("Urecognized tag:" ++ tag)


sendWebR : WebRCmd -> Cmd msg
sendWebR cmd =
    (case cmd of
        InitWebR ->
            [ ( "msg", E.string "init" ) ]

        GenerateImage ->
            [ ( "msg", E.string "gen" ) ]
    )
        |> E.object
        |> sendPort
