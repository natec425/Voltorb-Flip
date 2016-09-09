module AI.View exposing (..)

import Game.View
import AI.Core exposing (..)
import Game.Core exposing (Model(..), size, Board, Msg, allPoss)
import List.Extra
import Set

import Html exposing (Html, div, span, text, table, tr, td, button)
import Html.App
import Html.Events exposing (onClick)

view : AI.Core.Model -> Html AI.Core.Msg
view model =
    case model.gameModel of
        NoGame -> Game.View.view model.gameModel |> Html.App.map GameMsg
        Playing board ->
            div [] [
                Game.View.view model.gameModel |> Html.App.map GameMsg
                -- , viewExpectations board
                -- , action board |> toString |> text
                -- , playButton
                -- , autoPlayButton
                , div [] [text ("Wins: " ++ toString model.wins)]
                , div [] [text ("Loses: " ++ toString model.losses)]
            ]
        Won board -> Game.View.view model.gameModel |> Html.App.map GameMsg
        Lost board -> Game.View.view model.gameModel |> Html.App.map GameMsg

viewExpectations : Board -> Html AI.Core.Msg
viewExpectations board =
    table
        []
        (allPoss
         |> Set.toList
         |> List.Extra.groupsOf size
         |> List.map (viewRow board))

viewRow : Board -> List (Int, Int) -> Html AI.Core.Msg
viewRow board poss =
    tr []
        (poss
         |> List.map (viewCell board))

viewCell : Board -> (Int, Int) -> Html AI.Core.Msg
viewCell board (row, col) =
    td []
        [expectation board (row, col)
         |> \f -> toFloat (round (f * 100)) / 100.0
         |> toString
         |> text]

playButton : Html AI.Core.Msg
playButton = button [onClick Play] [text "Play AI"]

autoPlayButton : Html AI.Core.Msg
autoPlayButton = button [onClick AutoPlay] [text "Autoplay 1 Game"]