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
    case model of
        NoGame -> Game.View.view model |> Html.App.map GameMsg
        Playing board ->
            div [] [
                Game.View.view model |> Html.App.map GameMsg
                , viewExpectations board
                , action board |> toString |> text
                , playButton
            ]
        Won board -> Game.View.view model |> Html.App.map GameMsg
        Lost board -> Game.View.view model |> Html.App.map GameMsg

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