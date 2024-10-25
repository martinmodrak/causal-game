module GameState exposing (..)

import Association
import Crypto.Strings
import GameJson
import Json.Decode as D
import Json.Encode as E
import Random
import SingleRelationship
import ThreeWay
import TwoRelationships


type alias GameState =
    { association : Association.Model
    , singleRel : SingleRelationship.Model
    , twoRel : TwoRelationships.Model
    , threeWay : ThreeWay.Model
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
                    , ( "threeWay", GameJson.threeWayScenarioEncoder game.threeWay )
                    ]
                )

        encryption =
            Crypto.Strings.encrypt (Random.initialSeed rs) ps (Debug.log "XX" rawJson)
    in
    case encryption of
        Ok ( enc, _ ) ->
            E.string enc

        Err _ ->
            E.null


gameDecoder : D.Decoder GameState
gameDecoder =
    let
        rawDecoder =
            D.map4
                GameState
                (D.field "association" GameJson.associationScenarioDecoder)
                (D.field "singleRel" GameJson.singleRelScenarioDecoder)
                (D.field "twoRel" GameJson.twoRelScenarioDecoder)
                (D.field "threeWay" GameJson.threeWayScenarioDecoder)

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
        || hasDataFunc game.threeWay
