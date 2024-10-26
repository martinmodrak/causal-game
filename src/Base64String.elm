module Base64String exposing (..)

import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode


encode : String -> String
encode s =
    s
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
        |> Base64.fromBytes
        |> Maybe.withDefault "RXJyb3IgNTYyNw=="



-- s
--     |> String.toList
--     |> List.map Char.toCode
--     |> Base64.encode
--     |> Result.withDefault "RXJyb3IgNTYyNw=="


decode : String -> Result String String
decode s =
    s
        |> Base64.toBytes
        |> Maybe.andThen (\x -> Bytes.Decode.decode (Bytes.Decode.string (Bytes.width x)) x)
        |> Result.fromMaybe "Decoding error"



-- Base64.decode s
--     |> Result.map
--         (List.map Char.fromCode
--             >> String.fromList
--         )
