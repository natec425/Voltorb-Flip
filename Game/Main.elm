module Game.Main exposing (..)

import Game.Core exposing (init, update, subscriptions)
import Game.View exposing (view)

import Html.App

main : Program Never
main =
    Html.App.program
            { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions}

