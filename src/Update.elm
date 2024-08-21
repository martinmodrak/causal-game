module Update exposing (update)

import Generators exposing (associationGenerator)
import Random
import Types exposing (Model, Msg(..), Scenario(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScenarioGenerated sc ->
            ( { model | scenarios = { scenario = sc, data = [] } :: model.scenarios }, Cmd.none )

        GetData sc ->
            case sc of
                Association spec ->
                    ( model, Random.generate DataGenerated (associationGenerator spec 20) )

        DataGenerated data ->
            case model.scenarios of
                [] ->
                    ( model, Cmd.none )

                active :: rest ->
                    ( { model | scenarios = { active | data = data :: active.data } :: rest }, Cmd.none )
