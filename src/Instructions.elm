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
            valuesFromContingency 8 60 24 8

        ( noAssocSkewedX, noAssocSkewedY ) =
            valuesFromContingency 22 44 11 22

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
                [ li [] [ text " You are an exoepidemiologist. " ]
                , li [] [ text " You will be asked to assess possible relationships between various binary (true/false) properties measured for a bunch of aliens." ]
                , li [] [ text " The game is divided into multiple scenarios of increasing difficulty." ]
                , if Settings.homeworkEnabled then
                    li [] [ text " Homework is fulfilled by successfully handling the \"Homework\" scenario " ]

                  else
                    text ""
                , li [] [ text " You can complete multiple instances of each scenario. Instances differ in what alien you study and what the true relationships are, but have the same structure." ]
                , li [] [ text " In each instance, you can run several experiments and once you think you've learned enough, you make a guess about the causal relationships in the data." ]
                , li [] [ text " Running experiments costs money. You aim for high correctness of your guess while spending as little money as possible" ]
                ]
            ]
        , h2 [] [ text " Visualisation" ]
        , p []
            [ text " To let your eyes do \"informal statistics\" we present the data in a slightly unusual format closely related to the "
            , a [ Attr.href "https://en.wikipedia.org/wiki/Contingency_table" ] [ text " contingency table" ]
            , text " . So let us assume that we observed 100 aliens and exactly 25 were sleeping and moving, 25 were sleeping and not moving,"
            , text "  25 were not sleeping and moving and 25 were neither sleeping nor moving. I.e. there is no association between the two."
            , text " A contingency table for this case would look like this (note that we show ratios instead of the more common total sums per row/column): "
            ]
        , p [] [ Causality.viewSingleContingency allEqualX allEqualY "sleeping" "moving" ]
        , p []
            [ text " The uniformity of spacing between the dots in all quadrants indicates that indeed there is not much interesting going on."
            , text " What would however happen if there was a strong negative association, i.e. aliens that move are much less likely to sleep and vice versa? That's what we see in the image below:"
            ]
        , p []
            [ text " We see that this makes the upper-left and lower-right quadrants crowded while the other two quadrants have larger distances between points (or lower point density). Remember that each dot is a single alien."
            , text " Now let's say we investigate traits with positive association, \"moving\" and \"eyes open\", this could look something like this:"
            ]
        , p []
            [ text " We see that in this case there is more crowding in the lower-left and upper-right quadrants."
            , text " This visual evaluation of \"crowding\" or \"density\" is what you'll use for evaluation of associations in this game."
            ]
        , p []
            [ text " However, the data we've seen so far have been to neat - in both variables exactly 50% aliens produced \"True\"."
            , text "  The image below shows what happens when aliens spend only about third of their life sleeping and move a lot in general:"
            ]
        , p []
            [ text " Now the internal division lines have shifted to indicate that not sleeping is more prevalent than sleeping and moving is more prevalent than not moving (to be precise, the dividing lines indicate the overall proportion of sleeping/moving in the whole sample)."
            , text " We still however see an increased density in the upper left and lower right regions, indicating a negative association."
            , text " What if we study categories with overall uneven distribution but no association? Say we are studying the aliens inclination for math and hunger:"
            ]
        , p []
            [ text " The internal division lines are still shifted, but the overall distribution of points is uniform, indicating no relationship."
            , text "  Indeed when we look at the corresponding contingency table, we can compute that the ratio of hungry to non-hungry aliens is the same (1:2) among those that like math and those that do not like math at all."
            ]
        , p [] [ Causality.viewSingleContingency noAssocSkewedX noAssocSkewedY "likes math" "hungry" ]
        , p []
            [ text " So to summarise - roughly even spacing between points in all quadrants means there's likely no association. Overrepresentation on one of the diagonals than corresponds to either positive or negative association."
            , text "  That should be all you need to get started on the first task."
            ]
        ]
