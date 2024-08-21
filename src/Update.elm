module Update exposing (update)

import Associations exposing (associationGenerator)
import Random
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AssocMsg assocMsg ->
            let
                ( newAssocModel, assocCmd ) =
                    updateAssoc assocMsg model.association
            in
            ( { model | association = newAssocModel }, Cmd.map AssocMsg assocCmd )


updateAssoc : AssociationMsg -> AssociationModel -> ( AssociationModel, Cmd AssociationMsg )
updateAssoc msg model =
    case msg of
        AssocSpecGenerated spec ->
            ( { model | scenarios = { spec = spec, data = [] } :: model.scenarios }, Cmd.none )

        AssocRunExperiment ->
            case model.scenarios of
                head :: _ ->
                    ( model, Random.generate (AssocData model.proposedExperiment) (associationGenerator head.spec model.proposedExperiment) )

                [] ->
                    ( model, Cmd.none )

        AssocData experiment data ->
            case model.scenarios of
                [] ->
                    ( model, Cmd.none )

                active :: rest ->
                    ( { model | scenarios = { active | data = ( experiment, data ) :: active.data } :: rest }, Cmd.none )

        AssocSetN newN ->
            case String.toInt newN of
                Just n ->
                    ( { model | proposedExperiment = n }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )
