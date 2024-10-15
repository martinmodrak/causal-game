module Association exposing (..)

import Game exposing (GuessEval, Msg(..))
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy
import Random
import Random.Extra
import Random.Float
import Round
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


initAdapter : Game.InitAdapter Guess Experiment
initAdapter =
    { defaultGuess = False
    , defaultExperiment = 20
    , scenarioName = "Is there an association?"
    }


logicAdapter : Game.LogicAdapter ExpMsg GuessMsg Spec Experiment Outcome Guess
logicAdapter =
    { specGenerator = specGenerator
    , generator = generator
    , updateExperiment = updateExperiment
    , updateGuess = updateGuess
    , guessEval = guessEval
    , costExperiment = costExperiment
    }


viewAdapter : Game.ViewAdapter ExpMsg GuessMsg Spec Experiment Outcome Guess
viewAdapter =
    { viewExperiment = viewExperiment
    , viewProposedExperiment = viewProposedExperiment
    , viewCostCommentary = viewCostCommentary
    , viewGuess = viewGuess
    , viewProposedGuess = viewProposedGuess
    }


adapter : Game.Adapter ExpMsg GuessMsg Spec Experiment Outcome Guess
adapter =
    { init = initAdapter
    , logic =
        logicAdapter
    , view = viewAdapter
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


guessEval : Spec -> Guess -> GuessEval
guessEval spec guess =
    let
        xname =
            "x"

        yname =
            "y"
    in
    if guess == (abs spec.slope > maxAbsSlopeNoAssoc) then
        ( True, text "" )

    else
        ( False
        , span []
            [ text "The average change in "
            , em [] [ text yname ]
            , text " for 1 point increase in "
            , em [] [ text xname ]
            , text " was "
            , text (Round.round 2 spec.slope)
            ]
        )


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


viewExperiment : Spec -> ( Experiment, Outcome ) -> Html Never
viewExperiment _ ( experiment, data ) =
    div []
        [ div [] [ text ("N = " ++ String.fromInt experiment ++ ", CZK " ++ String.fromInt (costExperiment experiment)) ]
        , Html.Lazy.lazy View.vegaPlot (vegaSpec data)
        ]


viewProposedExperiment : Spec -> Experiment -> Html ExpMsg
viewProposedExperiment _ experiment =
    div []
        [ text "Run an observational study with "
        , View.nChooser SetN experiment
        , text " participants."
        ]


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
    text ("Observational study costs CZK " ++ String.fromInt costPerExperiment ++ " + CZK " ++ String.fromInt costPerParticipant ++ " per participant")


viewGuess : Spec -> Guess -> Html Never
viewGuess _ guess =
    let
        guessDescription =
            if guess then
                "There IS an association"

            else
                "NO association"
    in
    h3 [] [ text ("Your guess: " ++ guessDescription) ]


viewProposedGuess : Spec -> Guess -> Html GuessMsg
viewProposedGuess _ guess =
    let
        xname =
            "x"

        yname =
            "y"
    in
    div []
        [ text "I believe "
        , em [] [ text xname ]
        , text " "
        , select []
            [ option [ Attr.selected guess, Events.onClick (SetGuess True) ] [ text "IS associated" ]
            , option [ Attr.selected (not guess), Events.onClick (SetGuess False) ] [ text "is NOT associated" ]
            ]
        , text " with "
        , em [] [ text yname ]
        , text " (association means at least "
        , text (String.fromFloat maxAbsSlopeNoAssoc)
        , text " increase/decrease in "
        , em [] [ text yname ]
        , text " for 1 point change in "
        , em [] [ text yname ]
        , text "."
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
