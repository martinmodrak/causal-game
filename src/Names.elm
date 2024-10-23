module Names exposing (..)

import Random
import Random.List


things : List String
things =
    [ "rock"
    , "volcanic ash"
    , "starlight"
    , "uranium"
    , "beryllium"
    , "water"
    , "propylene"
    , "scissors"
    , "glyphosphate"
    , "unobtanium"
    , "spice"
    ]


verbs : List String
verbs =
    [ "eats ", "excretes ", "hit by ", "loves ", "scared of ", "near to " ]


nameGenerator : Int -> Random.Generator (List String)
nameGenerator n =
    let
        chosenVerbs =
            Random.List.shuffle verbs |> Random.map (List.take n)

        chosenThings =
            Random.List.shuffle things |> Random.map (List.take n)

        combinator =
            \l1 l2 -> List.map2 (++) l1 l2
    in
    Random.map2 combinator chosenVerbs chosenThings


placesHead =
    "Pluto"


places : List String
places =
    [ "Alpha Centauri"
    , "the Andromeda galaxy"
    , "Uranus"
    , "the asteroid belt"
    , "Neptune"
    , "BX 4851"
    , "the rings of Jupiter"
    , "the seas of Europa"
    , "Haumea"
    , "a strange dimension"
    , "Arcturus b"
    , "lifeless void"
    ]


creaturesHead =
    "blobs"


creatures : List String
creatures =
    [ "amoebas"
    , "cephalopods"
    , "locusts"
    , "spiders"
    , "creepers"
    , "moss"
    , "tree-shaped-maniacs"
    , "orchids"
    , "cultists"
    ]


attributesHead =
    "Dark"


attributes : List String
attributes =
    [ "Skinny"
    , "Monstrous"
    , "Tiny"
    , "Semi-frozen"
    , "Ambitious"
    , "Ephemeral"
    , "Slippery"
    , "Fluffy"
    , "Dangerous"
    , "Screaming"
    ]


creatureNameGenerator : Random.Generator String
creatureNameGenerator =
    let
        combiner =
            \attr crit pl ->
                attr ++ " " ++ crit ++ " from " ++ pl
    in
    Random.map3 combiner
        (Random.uniform attributesHead attributes)
        (Random.uniform creaturesHead creatures)
        (Random.uniform placesHead places)



-- vars : List ( String, List String )
-- vars =
--     [ ( "eats "
--       , [ "scissors" ]
--       ),
--       ( "excretes ",
--         [ " Pocahontas"]
--       ),
--       ( "hit by ")
--     ]
