module Game.View exposing (..)

import Game.Core exposing (..)


import Html exposing (Html, div, text, table,  tr, td, span, button, node)
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, href, rel)
import Set
import Array

-- HELPERS

link : List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
link atts children =
    node "link" atts children

-- VIEW

view : Model -> Html Msg
view model =
    case model of
        NoGame -> restartButton
        Playing board -> viewBoard board
        Won board -> 
            div [] [ text "You Win!"
                   , viewBoard {board | exposed = allPoss} ]
        Lost board ->
            div [] [ text "You Lose!"
                   , viewBoard {board | exposed = allPoss}]

viewBoard : Board -> Html Msg
viewBoard board =
    div [baseStyle]
        [ link [href "https://fonts.googleapis.com/css?family=Roboto", rel "stylesheet" ] []
        , div [] [ viewMineField board
                 , viewRowSummaries board ]
        , viewColSummaries board 
        , restartButton ]

viewScore : Board -> Html Msg
viewScore board =
    div [] [text ("Your Score: " ++ (toString (score board)))]

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
          then let v = cellValue board row column
               in (if v == 0 then "X" else toString v) |> text
          else text "?" ]

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
