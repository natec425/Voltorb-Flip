module AI.View exposing (..)

import Game.View
import AI.Core exposing (..)
import Game.Core exposing (Model(..), size, Board, Msg, allPoss)
import List.Extra
import Set

import Html exposing (Html, div, span, text, table, tr, td)

view : AI.Core.Model -> Html Msg
view model =
    case model of
        NoGame -> Game.View.view model
        Playing board ->
            div [] [
                Game.View.view model
                , viewExpectations board
                , action board |> toString |> text
            ]
        Won board -> Game.View.view model
        Lost board -> Game.View.view model

viewExpectations : Board -> Html Msg
viewExpectations board =
    table
        []
        (allPoss
         |> Set.toList
         |> List.Extra.groupsOf size
         |> List.map (viewRow board))

viewRow : Board -> List (Int, Int) -> Html Msg
viewRow board poss =
    tr []
        (poss
         |> List.map (viewCell board))

viewCell : Board -> (Int, Int) -> Html Msg
viewCell board (row, col) =
    td []
        [expectation board (row, col)
         |> \f -> toFloat (round (f * 100)) / 100.0
         |> toString
         |> text]
