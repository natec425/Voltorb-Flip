module AI.View exposing (..)

import Game.View
import AI.Core exposing (..)
import Html exposing (Html, div, span, text, button)
import Html.App
import Html.Events exposing (onClick)


view : Model a -> Html Msg
view model =
    div []
        [ Game.View.view model.gameModel |> Html.App.map GameMsg
        , autoPlayButton model
        , div [] [ text ("Wins: " ++ toString model.wins) ]
        , div [] [ text ("Loses: " ++ toString model.losses) ]
        , div [] [ text ("Points Earned: " ++ toString model.points) ]
        ]


autoPlayButton : Model a -> Html Msg
autoPlayButton model =
    button [ onClick AutoPlay ]
        [ if model.playing then
            text "Turn Off AutoPlay"
          else
            text "Turn On AutoPlay"
        ]
