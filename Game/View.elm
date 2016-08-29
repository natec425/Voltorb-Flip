module Game.View exposing (..)

import Game.Core exposing (Board, Msg(..), size)


import Html exposing (Html, div, text, table,  tr, td, span, button, node)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, href, rel)
import Set
import Dict
import Array

-- HELPERS

link : List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
link atts children =
    node "link" atts children

-- VIEW

view : Board -> Html Msg
view board =
    div [baseStyle]
        [ link [href "https://fonts.googleapis.com/css?family=Roboto", rel "stylesheet" ] []
        , viewWin board
        , div [] [ viewMineField board
                 , viewRowSummaries board ]
        , viewColSummaries board 
        , restartButton ]

viewScore : Board -> Html Msg
viewScore board =
    div [] [text ("Your Score: " ++ (toString (score board)))]

score : Board -> Int
score board =
    if Set.isEmpty board.exposed
    then 0
    else board.exposed
        |> Set.toList
        |> List.map (\p -> Dict.get p board.targets |> Maybe.withDefault 1)
        |> List.product

viewWin : Board -> Html Msg
viewWin board =
    span [] [
        if not (Set.intersect board.exposed board.mines |> Set.isEmpty)
        then text "You Lose!"
        else if Set.diff (board.targets |> Dict.keys |> Set.fromList) board.exposed |> Set.isEmpty
        then span [] [viewScore board, text "You Win!"]
        else viewScore board
    ]

viewMineField : Board -> Html Msg
viewMineField board =
    div [minefieldStyle] [
        table [borderCollapse] (Array.initialize size (viewRow board) |> Array.toList)
    ]

viewRow : Board -> Int -> Html Msg
viewRow board row =
    tr [] (Array.initialize size (viewCell board row) |> Array.toList)

viewCell : Board -> Int -> Int -> Html Msg
viewCell board row column =
    td [cellStyle, onClick (Expose row column)]
        [ if Set.member (row, column) board.exposed
          then text (cellValue board row column)
          else text "?" ]

cellValue : Board -> Int -> Int -> String
cellValue board row column =
    if Set.member (row, column) board.mines
    then "X"
    else case Dict.get (row, column) board.targets of
            Just v -> toString v
            Nothing -> "1"

viewRowSummaries : Board -> Html Msg
viewRowSummaries board =
    div [rowSummariesStyle] [
        table [borderCollapse] [ tr [] (Array.initialize size (viewRowSummary board) |> Array.toList) ]
    ]

viewRowSummary : Board -> Int -> Html Msg
viewRowSummary board row =
    td [rowSummaryStyle]
        [ span [pointStyle] [text (toString (rowPoints board row))]
        , span [mineStyle] [text (toString (rowMines board row))] ]

rowPoints : Board -> Int -> Int
rowPoints board row =
    [0, 1, 2, 3, 4]
    |> List.filter (\c -> not (Set.member (row, c) board.mines))
    |> List.map (\c -> Dict.get (row, c) board.targets)
    |> List.map (Maybe.withDefault 1)
    |> List.sum

rowMines : Board -> Int -> Int
rowMines board row =
    Set.filter (\(r, c) -> r == row) board.mines |> Set.size

viewColSummaries : Board -> Html Msg
viewColSummaries board =
    div [colSummariesStyle] [
        table [borderCollapse] [ tr [] (Array.initialize size (viewColSummary board) |> Array.toList) ]
    ]

viewColSummary : Board -> Int -> Html Msg
viewColSummary board col =
    td [colSummaryStyle]
        [ span [pointStyle] [text (toString (colPoints board col))]
        , span [mineStyle] [text (toString (colMines board col))]]

colPoints : Board -> Int -> Int
colPoints board col =
    [0, 1, 2, 3, 4]
    |> List.filter (\r -> not (Set.member (r, col) board.mines))
    |> List.map (\r -> Dict.get (r, col) board.targets)
    |> List.map (Maybe.withDefault 1)
    |> List.sum

colMines : Board -> Int -> Int
colMines board col =
    Set.filter (\(r, c) -> col == c) board.mines |> Set.size

restartButton : Html Msg
restartButton =
    button [onClick NewGame ]
         [ text "Click to Restart" ]

-- STYLES

baseStyle : Html.Attribute Msg
baseStyle =
    style [
        ("font-family", "'Roboto', sans-serif")
        , ("box-sizing", "content-box")
        , ("-moz-box-sizing", "content-box")
        , ("-webkit-box-sizing", "content-box")
    ]

borderCollapse : Html.Attribute Msg
borderCollapse =
    style [("border-collapse", "collapse")]

minefieldStyle : Html.Attribute Msg
minefieldStyle =
    style [ ("float", "left")]

cellStyle : Html.Attribute Msg
cellStyle =
    style [
        ("padding", ".5em")
        , ("background", "#00cc66")
        , ("width", "2.5em")
        , ("height", "2.5em")
        , ("text-align", "center")
    ]

rowSummariesStyle : Html.Attribute Msg
rowSummariesStyle =
    style [
        ("margin", "0px")
    ]

rowSummaryStyle : Html.Attribute Msg
rowSummaryStyle =
    style [
        ("display", "block")
        , ("background", "#DDDDDD")
        , ("padding", ".5em")
        , ("width", "2.5em")
        , ("height", "2.5em")
        , ("text-align", "center")
    ]

colSummariesStyle : Html.Attribute Msg
colSummariesStyle =
    style []

colSummaryStyle : Html.Attribute Msg
colSummaryStyle =
    style [
        ("background", "#DDDDDD")
        , ("padding", ".5em")
        , ("width", "2.5em")
        , ("height", "2.5em")
        , ("text-align", "center")
    ]

pointStyle : Html.Attribute Msg
pointStyle =
    style [
        ("color", "green")
        , ("display", "block")
    ]

mineStyle : Html.Attribute Msg
mineStyle =
    style [
        ("color", "red")
    ]
