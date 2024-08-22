module Association exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Random
import Random.Extra
import Random.Float
import VegaLite as VL
import View


type Msg
    = SpecGenerated Spec
    | RunExperiment
    | DataGenerated Experiment VL.Data
    | SetN String
    | MakeGuess Guess
    | NewScenario


type alias Spec =
    { slope : Float
    , intercept : Float
    , noise : Float
    }


type alias Experiment =
    Int


type alias Guess =
    Bool


type alias Scenario =
    { spec : Spec
    , data : List ( Experiment, VL.Data )
    , guess : Maybe Guess
    }


type alias Model =
    { scenarios : List Scenario
    , proposedExperiment : Experiment
    }


initModel : Model
initModel =
    { scenarios = [], proposedExperiment = 20 }


initCmd : Cmd Msg
initCmd =
    Random.generate SpecGenerated specGenerator


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpecGenerated spec ->
            ( { model | scenarios = { spec = Debug.log "Spec: " spec, data = [], guess = Nothing } :: model.scenarios }, Cmd.none )

        RunExperiment ->
            case model.scenarios of
                head :: _ ->
                    ( model, Random.generate (DataGenerated model.proposedExperiment) (generator head.spec model.proposedExperiment) )

                [] ->
                    ( model, Cmd.none )

        DataGenerated experiment data ->
            case model.scenarios of
                [] ->
                    ( model, Cmd.none )

                active :: rest ->
                    ( { model | scenarios = { active | data = ( experiment, data ) :: active.data } :: rest }, Cmd.none )

        SetN newN ->
            case String.toInt newN of
                Just n ->
                    ( { model | proposedExperiment = n }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        MakeGuess guess ->
            case model.scenarios of
                [] ->
                    ( model, Cmd.none )

                active :: rest ->
                    ( { model | scenarios = { active | guess = Just guess } :: rest }, Cmd.none )

        NewScenario ->
            ( model, Random.generate SpecGenerated specGenerator )


view : Model -> Html Msg
view model =
    View.scenariosList (viewSingleScenario model.proposedExperiment) model.scenarios


viewSingleScenario : Experiment -> Bool -> Scenario -> Html Msg
viewSingleScenario experiment isactive scModel =
    View.scenario
        (viewInfo scModel.guess isactive experiment scModel.spec)
        (List.map (viewSingleScenarioData scModel.spec) scModel.data)


viewSingleScenarioData : Spec -> ( Experiment, VL.Data ) -> Html Msg
viewSingleScenarioData spec ( _, data ) =
    View.vegaPlot (vegaSpec spec data)


viewInfo : Maybe Guess -> Bool -> Experiment -> Spec -> List (Html Msg)
viewInfo guess isactive experiment spec =
    let
        ( wasGuessed, guessElement ) =
            case guess of
                Just guessVal ->
                    ( True, viewGuess isactive guessVal spec )

                Nothing ->
                    ( False, text "" )

        activeElement =
            if not wasGuessed && isactive then
                div []
                    [ View.nChooser SetN experiment
                    , input [ Attr.type_ "button", Events.onClick RunExperiment, Attr.value "Gather more data" ] []
                    , h3 [] [ text "Ready to make a guess?" ]
                    , input [ Attr.type_ "button", Events.onClick (MakeGuess True), Attr.value "There is association" ] []
                    , input [ Attr.type_ "button", Events.onClick (MakeGuess False), Attr.value "There is no association beyond XX" ] []
                    ]

            else
                text ""
    in
    [ h2 [] [ text "Is there an association?" ]
    , activeElement
    , guessElement
    ]


viewGuess : Bool -> Guess -> Spec -> Html Msg
viewGuess isactive guess spec =
    let
        guessDescription =
            if guess then
                "There IS an association"

            else
                "NO association"

        guessResultDescription =
            if guessCorrect spec guess then
                "Correct"

            else
                "Incorrect"

        newScenarioElement =
            if isactive then
                div [] [ input [ Attr.type_ "button", Events.onClick NewScenario, Attr.value "New scenario" ] [] ]

            else
                text ""
    in
    div []
        [ h3 [] [ text ("Your guess:" ++ guessDescription) ]
        , text "The guess was "
        , strong [] [ text guessResultDescription ]
        , newScenarioElement
        ]


vegaSpec : Spec -> VL.Data -> VL.Spec
vegaSpec _ data =
    let
        enc =
            VL.encoding
                << VL.position VL.X [ VL.pName "x", VL.pQuant ]
                << VL.position VL.Y [ VL.pName "y", VL.pQuant ]
    in
    VL.toVegaLite [ data, enc [], VL.circle [] ]
