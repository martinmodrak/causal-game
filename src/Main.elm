module Main exposing (..)

import Browser
import Types exposing (Model, Msg(..), WebRCmd(..))
import Update exposing (update)
import View exposing (view)
import WebR exposing (receiveSub, sendWebR)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { imgData = Nothing
      , webRReady = False
      , lastError = Nothing
      }
    , Cmd.batch
        [ sendWebR InitWebR
        ]
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            always
                (Sub.batch
                    [ receiveSub
                    ]
                )
        }
