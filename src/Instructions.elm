module Instructions exposing (..)

import Causality
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy
import Settings


valuesFromContingency : Int -> Int -> Int -> Int -> ( List Bool, List Bool )
valuesFromContingency ff ft tf tt =
    ( List.repeat (ff + ft) False ++ List.repeat (tf + tt) True
    , List.repeat ff False ++ List.repeat ft True ++ List.repeat tf False ++ List.repeat tt True
    )


view : Html Never
view =
    let
        ( allEqualX, allEqualY ) =
            valuesFromContingency 25 25 25 25

        ( negEvenX, negEvenY ) =
            valuesFromContingency 10 40 40 10

        ( posEvenX, posEvenY ) =
            valuesFromContingency 35 15 15 35

        ( negSkewedX, negSkewedY ) =
            valuesFromContingency 8 60 24 10

        ( noAssocSkewedX, noAssocSkewedY ) =
            valuesFromContingency 24 43 11 22

        exBeehiveX =
            List.append (List.repeat 20 True) (List.repeat 20 False)

        exBeehiveY =
            [ -0.1, -0.01, -2.1, -1.62, -0.34, 0.4, 0.44, -1.27, -0.14, 0.06, 1.05, 1.36, -0.64, -1.06, 1.03, -1.8, -0.31, -0.54, -1.47, -0.44, 0.31, 1.24, 2.84, 1.89, 1.38, 0.92, 1.1, 1.74, -1.22, 1.9, 0.69, 0.76, -0.27, -0.22, 0.79, 0.52, 1.64, 2.81, 1.53, 1.73 ]

        -- ( testX, testY ) =
        --     valuesFromContingency 0 49 49 0
        -- ( test2X, test2Y ) =
        --     valuesFromContingency 16 117 13 6
        -- Positive assoc example
        -- ( testX, testY ) =
        --     valuesFromContingency 25 0 0 25
        -- ( test2X, test2Y ) =
        --     valuesFromContingency 75 10 30 40
        -- No association example
        -- ( testX, testY ) =
        --     valuesFromContingency 16 16 16 16
        -- ( test2X, test2Y ) =
        --     valuesFromContingency 75 25 30 10
        -- Problematic data example
        -- ( testX, testY ) =
        --     valuesFromContingency 16 4 62 18
        -- ( test2X, test2Y ) =
        --     valuesFromContingency 73 13 6 8
    in
    div [ Attr.class "scenario", Attr.class "instructions" ]
        [ h2 []
            [ text " The big picture" ]

        -- , p [] [ Causality.viewSingleWaffle testX testY "young" "breastfeeding" ]
        -- , p [] [ Causality.viewSingleWaffle test2X test2Y "wounded" "fast" ]
        , p []
            [ ul []
                [ li [] [ text " This homework aims to show some of the differences between randomized and observational studies and let you attempt causal thinking. " ]
                , li [] [ text " You play an exoepidemiologist. " ]
                , li [] [ text " You will be asked to assess possible relationships between a single continuous outcome and one or two binary (true/false) properties measured for a bunch of aliens." ]
                , li [] [ text " The game is divided into multiple scenarios of increasing difficulty." ]
                , if Settings.homeworkEnabled then
                    li [] [ text " Homework is fulfilled by successfully handling the \"Homework\" scenario, downloading a results file and sending it by e-mail. " ]

                  else
                    text ""
                , li [] [ text " You can complete multiple instances of each scenario. Instances differ in what alien you study and what the true relationships are, but have the same structure." ]
                , li [] [ text " In each instance, you can run several experiments and once you think you've learned enough, you make a guess about the causal relationships in the data." ]
                , li [] [ text " Running experiments costs money. You aim for high correctness of your guess while spending as little money as possible" ]
                ]
            ]
        , h2 [] [ text " Data displays" ]
        , p []
            [ text " We expect yout to do some \"informal statistics\"  - for continuous data, we show the observed numbers split by binary variable levels, along with means and 95% confidence intervals (black)." ]
        , p [] [ Causality.viewSingleBeehive exBeehiveX exBeehiveY "sleeping" "speed" ]
        , p []
            [ text "For relationships between two binary outcomes we show a "
            , a [ Attr.href "https://en.wikipedia.org/wiki/Contingency_table" ] [ text " contingency table." ]
            , text " If you are not familiar with contingency tables, we will do a small refresh here."
            ]
        , p []
            [ text "So let us assume that we observed 100 aliens and exactly 25 were sleeping and moving, 25 were sleeping and not moving,"
            , text "  25 were not sleeping and moving and 25 were neither sleeping nor moving. I.e. there is no association between the two."
            , text " A contingency table for this case would look like this (note that we show ratios instead of the more common total sums per row/column): "
            ]
        , p [] [ Causality.viewSingleContingency allEqualX allEqualY "sleeping" "moving" ]
        , p []
            [ text " What would however happen if there was a strong negative association, i.e. aliens that move are much less likely to sleep and vice versa? That's what we see in the table below:"
            ]
        , p [] [ Causality.viewSingleContingency negEvenX negEvenY "sleeping" "moving" ]
        , p []
            [ text " We see that this makes the upper-left and lower-right cells show high numbers while the other two cells have lower counts. The ratios are also uneven across rows/columns"
            , text " Now let's say we investigate traits with positive association, \"moving\" and \"eyes open\", this could look something like this:"
            ]
        , p [] [ Causality.viewSingleContingency posEvenX posEvenY "eyes open" "moving" ]
        , p []
            [ text " We see that in this case there are high counts in the lower-left and upper-right quadrants."
            ]
        , p []
            [ text " However, the data we've seen so far have been to neat - in both variables exactly 50% aliens produced \"True\"."
            , text "  The table below shows what happens when aliens spend only about third of their life sleeping and move a lot in general:"
            ]
        , p [] [ Causality.viewSingleContingency negSkewedX negSkewedY "sleeping" "moving" ]
        , p []
            [ text " The numbers are different, but we still see increased counts in the upper left and lower right cells and wildly different ratios, indicating a negative association."
            , text " What if we study categories with overall uneven distribution but no association? Say we are studying the aliens inclination for math and hunger:"
            , text "  We will see that the ratio of hungry to non-hungry aliens is approximately the same among those that like math and those that do not like math at all."
            ]
        , p [] [ Causality.viewSingleContingency noAssocSkewedX noAssocSkewedY "likes math" "hungry" ]
        , p []
            [ text " So to summarise - similar ratios across rows/columns mean there's likely no association. High counts on one of the diagonals than correspond to either positive or negative association."
            , text "  That should be all you need to get started on the first task."
            ]
        ]
