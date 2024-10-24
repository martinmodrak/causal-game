module GameState exposing (..)

import Association
import Crypto.Strings
import GameJson
import Json.Decode as D
import Json.Encode as E
import Random
import SingleRelationship
import TwoRelationships


type alias GameState =
    { association : Association.Model
    , singleRel : SingleRelationship.Model
    , twoRel : TwoRelationships.Model
    }


rs : Int
rs =
    168555


ps : String
ps =
    "Observational study costs CZK"


gameEncoder : GameState -> E.Value
gameEncoder game =
    let
        rawJson =
            E.encode 0
                (E.object
                    [ ( "association", GameJson.associationScenarioEncoder game.association )
                    , ( "singleRel", GameJson.singleRelScenarioEncoder game.singleRel )
                    , ( "twoRel", GameJson.twoRelScenarioEncoder game.twoRel )
                    ]
                )

        encryption =
            Crypto.Strings.encrypt (Random.initialSeed rs) ps rawJson
    in
    case encryption of
        Ok ( enc, _ ) ->
            E.string (Debug.log "XX" enc)

        Err _ ->
            E.null


gameDecoder : D.Decoder GameState
gameDecoder =
    let
        rawDecoder =
            D.map3
                GameState
                (D.field "association" GameJson.associationScenarioDecoder)
                (D.field "singleRel" GameJson.singleRelScenarioDecoder)
                (D.field "twoRel" GameJson.twoRelScenarioDecoder)

        decryption =
            \enc -> Crypto.Strings.decrypt ps enc
    in
    D.string
        |> D.andThen
            (\enc ->
                case decryption enc of
                    Ok dec ->
                        case D.decodeString rawDecoder dec of
                            Ok val ->
                                D.succeed val

                            Err err ->
                                D.fail (D.errorToString err)

                    Err decErr ->
                        D.fail ("Decryption err: " ++ decErr)
            )


gameHasData : GameState -> Bool
gameHasData game =
    let
        hasDataFunc =
            \sc ->
                case sc.history of
                    _ :: _ :: _ ->
                        True

                    singleInst :: [] ->
                        not (List.isEmpty singleInst.data)

                    [] ->
                        False
    in
    hasDataFunc game.association
        || hasDataFunc game.singleRel
        || hasDataFunc game.twoRel
