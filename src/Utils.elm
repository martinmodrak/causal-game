module Utils exposing (..)


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1

    else
        0


boolToFloat : Bool -> Float
boolToFloat b =
    if b then
        1.0

    else
        0.0


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


boolFromString : String -> Bool
boolFromString s =
    s == "True"


safeAverage : List Int -> Float
safeAverage x =
    toFloat (List.sum x) / max 1.0 (toFloat (List.length x))


safeAverageF : List Float -> Float
safeAverageF x =
    List.sum x / max 1.0 (toFloat (List.length x))


triplet : a -> b -> c -> ( a, b, c )
triplet a b c =
    ( a, b, c )
