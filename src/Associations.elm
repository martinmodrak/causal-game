module Associations exposing (..)

import Random
import Random.Float
import Types exposing (AssociationSpec)
import VegaLite exposing (Data, dataColumn, dataFromColumns, nums)


associationSpecGenerator : Random.Generator AssociationSpec
associationSpecGenerator =
    Random.constant
        { slope = 1
        , intercept = 1
        , noise = 1
        }


associationGenerator : AssociationSpec -> Int -> Random.Generator Data
associationGenerator spec n =
    let
        x =
            Random.list n Random.Float.standardNormal

        ynoise =
            Random.list n (Random.Float.normal 0 spec.noise)
    in
    Random.map2 (assocFromXAndNoise spec) x ynoise


assocFromXAndNoise : AssociationSpec -> List Float -> List Float -> Data
assocFromXAndNoise spec x ynoise =
    let
        y =
            List.map2 (\xval noiseval -> spec.intercept + spec.slope * xval + noiseval) x ynoise
    in
    (dataFromColumns []
        << dataColumn "x" (nums x)
        << dataColumn "y" (nums y)
    )
        []
