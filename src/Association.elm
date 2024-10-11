module Association exposing (..)

import Game exposing (Msg(..))
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy
import Random
import Random.Extra
import Random.Float
import VegaLite as VL
import View


type alias Spec =
    { slope : Float
    , intercept : Float
    , noise : Float
    }


type alias Experiment =
    Int


type alias Guess =
    Bool


type alias Outcome =
    VL.Data


type ExpMsg
    = SetN String


type GuessMsg
    = SetGuess Guess


type alias Msg =
    Game.Msg ExpMsg GuessMsg Spec Experiment Outcome Guess


type alias Model =
    Game.Scenario Spec Experiment Outcome Guess


adapter : Game.Adapter ExpMsg GuessMsg Spec Experiment Outcome Guess
adapter =
    { defaultGuess = False
    , defaultExperiment = 20
    , specGenerator = specGenerator
    , generator = generator
    , updateExperiment = updateExperiment
    , updateGuess = updateGuess
    , guessCorrect = guessCorrect
    , costExperiment = costExperiment
    , viewExperiment = viewExperiment
    , viewProposedExperiment = viewProposedExperiment
    , viewCostCommentary = viewCostCommentary
    , viewGuess = viewGuess
    , viewProposedGuess = viewProposedGuess
    , scenarioName = "Is there an association?"
    }


specGenerator : Random.Generator Spec
specGenerator =
    Random.map3 Spec
        slopeGenerator
        (Random.map (\x -> abs x + 10) (Random.Float.normal 0 10))
        (Random.map (\x -> abs x + 2) (Random.Float.normal 0 5))


maxAbsSlopeNoAssoc : Float
maxAbsSlopeNoAssoc =
    0.5


slopeGenerator : Random.Generator Float
slopeGenerator =
    let
        genNoAssoc =
            Random.float -maxAbsSlopeNoAssoc maxAbsSlopeNoAssoc

        genAssoc =
            Random.map2 (*) (Random.Extra.choice -1 1) (Random.float 2 6)
    in
    Random.Extra.choices genAssoc [ genNoAssoc ]


generator : Spec -> Int -> Random.Generator VL.Data
generator spec n =
    let
        x =
            Random.list n Random.Float.standardNormal

        ynoise =
            Random.list n (Random.Float.normal 0 spec.noise)
    in
    Random.map2 (fromXAndNoise spec) x ynoise


fromXAndNoise : Spec -> List Float -> List Float -> VL.Data
fromXAndNoise spec x ynoise =
    let
        y =
            List.map2 (\xval noiseval -> spec.intercept + spec.slope * xval + noiseval) x ynoise
    in
    (VL.dataFromColumns []
        << VL.dataColumn "x" (VL.nums x)
        << VL.dataColumn "y" (VL.nums y)
    )
        []


guessCorrect : Spec -> Guess -> Bool
guessCorrect spec guess =
    guess == (abs spec.slope > maxAbsSlopeNoAssoc)


updateExperiment : ExpMsg -> Experiment -> Experiment
updateExperiment msg experiment =
    case msg of
        SetN newN ->
            case String.toInt newN of
                Just n ->
                    n

                Nothing ->
                    experiment


updateGuess : GuessMsg -> Guess -> Guess
updateGuess msg _ =
    case msg of
        SetGuess g ->
            g


viewExperiment : ( Experiment, Outcome ) -> Html ExpMsg
viewExperiment ( experiment, data ) =
    div []
        [ div [] [ text ("N = " ++ String.fromInt experiment ++ ", CZK " ++ String.fromInt (costExperiment experiment)) ]
        , Html.Lazy.lazy View.vegaPlot (vegaSpec data)
        ]


viewProposedExperiment : Experiment -> Html ExpMsg
viewProposedExperiment experiment =
    View.nChooser SetN experiment


costPerParticipant : Int
costPerParticipant =
    100


costPerExperiment : Int
costPerExperiment =
    2000


costExperiment : Experiment -> Int
costExperiment exp =
    exp * costPerParticipant + costPerExperiment


viewCostCommentary : Html a
viewCostCommentary =
    text ("Costs CZK " ++ String.fromInt costPerExperiment ++ " + CZK " ++ String.fromInt costPerParticipant ++ " per participant")


viewGuess : Guess -> Html GuessMsg
viewGuess guess =
    let
        guessDescription =
            if guess then
                "There IS an association"

            else
                "NO association"
    in
    h3 [] [ text ("Your guess: " ++ guessDescription) ]


viewProposedGuess : Guess -> Html GuessMsg
viewProposedGuess guess =
    div []
        [ text "I believe x "
        , select []
            [ option [ Attr.selected guess, Events.onClick (SetGuess True) ] [ text "IS associated" ]
            , option [ Attr.selected (not guess), Events.onClick (SetGuess False) ] [ text "is NOT associated" ]
            ]
        , text " with y"
        ]


vegaSpec : Outcome -> VL.Spec
vegaSpec data =
    let
        enc =
            VL.encoding
                << VL.position VL.X [ VL.pName "x", VL.pQuant ]
                << VL.position VL.Y [ VL.pName "y", VL.pQuant ]
    in
    VL.toVegaLite [ data, enc [], VL.circle [] ]
