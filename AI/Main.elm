module AI.Main exposing (..)

import AI.Core exposing (init, update, subscriptions)
import AI.View exposing (view)
import Html.App


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
