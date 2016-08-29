module Game.View exposing (..)

import Game.Core exposing (Model, Msg(..), size)


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

view : Model -> Html Msg
view model =
    div [baseStyle]
        [ link [href "https://fonts.googleapis.com/css?family=Roboto", rel "stylesheet" ] []
        , viewWin model
        , div [] [ viewMineField model
                 , viewRowSummaries model ]
        , viewColSummaries model 
        , restartButton ]

viewScore : Model -> Html Msg
viewScore model =
    div [] [text ("Your Score: " ++ (toString (score model)))]

score : Model -> Int
score model =
    if Set.isEmpty model.exposed
    then 0
    else model.exposed
        |> Set.toList
        |> List.map (\p -> Dict.get p model.targets |> Maybe.withDefault 1)
        |> List.product

viewWin : Model -> Html Msg
viewWin model =
    span [] [
        if not (Set.intersect model.exposed model.mines |> Set.isEmpty)
        then text "You Lose!"
        else if Set.diff (model.targets |> Dict.keys |> Set.fromList) model.exposed |> Set.isEmpty
        then span [] [viewScore model, text "You Win!"]
        else viewScore model
    ]

viewMineField : Model -> Html Msg
viewMineField model =
    div [minefieldStyle] [
        table [borderCollapse] (Array.initialize size (viewRow model) |> Array.toList)
    ]

viewRow : Model -> Int -> Html Msg
viewRow model row =
    tr [] (Array.initialize size (viewCell model row) |> Array.toList)

viewCell : Model -> Int -> Int -> Html Msg
viewCell model row column =
    td [cellStyle, onClick (Expose row column)]
        [ if Set.member (row, column) model.exposed
          then text (cellValue model row column)
          else text "?" ]

cellValue : Model -> Int -> Int -> String
cellValue model row column =
    if Set.member (row, column) model.mines
    then "X"
    else case Dict.get (row, column) model.targets of
            Just v -> toString v
            Nothing -> "1"

viewRowSummaries : Model -> Html Msg
viewRowSummaries model =
    div [rowSummariesStyle] [
        table [borderCollapse] [ tr [] (Array.initialize size (viewRowSummary model) |> Array.toList) ]
    ]

viewRowSummary : Model -> Int -> Html Msg
viewRowSummary model row =
    td [rowSummaryStyle]
        [ span [pointStyle] [text (toString (rowPoints model row))]
        , span [mineStyle] [text (toString (rowMines model row))] ]

rowPoints : Model -> Int -> Int
rowPoints model row =
    [0, 1, 2, 3, 4]
    |> List.filter (\c -> not (Set.member (row, c) model.mines))
    |> List.map (\c -> Dict.get (row, c) model.targets)
    |> List.map (Maybe.withDefault 1)
    |> List.sum

rowMines : Model -> Int -> Int
rowMines model row =
    Set.filter (\(r, c) -> r == row) model.mines |> Set.size

viewColSummaries : Model -> Html Msg
viewColSummaries model =
    div [colSummariesStyle] [
        table [borderCollapse] [ tr [] (Array.initialize size (viewColSummary model) |> Array.toList) ]
    ]

viewColSummary : Model -> Int -> Html Msg
viewColSummary model col =
    td [colSummaryStyle]
        [ span [pointStyle] [text (toString (colPoints model col))]
        , span [mineStyle] [text (toString (colMines model col))]]

colPoints : Model -> Int -> Int
colPoints model col =
    [0, 1, 2, 3, 4]
    |> List.filter (\r -> not (Set.member (r, col) model.mines))
    |> List.map (\r -> Dict.get (r, col) model.targets)
    |> List.map (Maybe.withDefault 1)
    |> List.sum

colMines : Model -> Int -> Int
colMines model col =
    Set.filter (\(r, c) -> col == c) model.mines |> Set.size

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
