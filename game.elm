module Game exposing (..)

import Html exposing (Html, div, text, table,  tr, td, span, button, node)
import Html.App
import Html.Attributes exposing (style, href, rel)
import Html.Events exposing (onClick)
import Set
import Array
import Dict
import Random
import Random.Array exposing (shuffle)

-- HELPERS

arraySwap : Array.Array a -> Int -> Int -> Maybe.Maybe (Array.Array a)
arraySwap a i j =
    ((Array.get j a)
    |> Maybe.map (\aj -> Array.set i aj a))
    `Maybe.andThen` (\a' -> Array.get i a 
                            |> Maybe.map (\ai -> Array.set j ai a')) 

listHalves : List a -> (List a, List a)
listHalves l =
    let len = round ((List.length l |> toFloat) / 2)
    in (List.take len l, List.drop len l)

listZip : List a -> List b -> List (a, b)
listZip a b =
    List.map2 (\l r -> (l, r)) a b

randomConst : a -> Random.Generator a
randomConst a = Random.bool |> Random.map (\_ -> a)

randomSample : Int -> Array.Array a -> Random.Generator (Array.Array a)
randomSample n l =
    shuffle l
    |> Random.map (Array.slice 0 n)

link : List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
link atts children =
    node "link" atts children

-- MODEL

size = 5

allPoss =
    [ (0, 0), (1, 0), (2, 0), (3, 0), (4, 0)
    , (0, 1), (1, 1), (2, 1), (3, 1), (4, 1)
    , (0, 2), (1, 2), (2, 2), (3, 2), (4, 2)
    , (0, 3), (1, 3), (2, 3), (3, 3), (4, 3)
    , (0, 4), (1, 4), (2, 4), (3, 4), (4, 4)]
    |> Set.fromList

type alias Model =
    { mines : Set.Set (Int, Int)
    , exposed : Set.Set (Int, Int)
    , targets : Dict.Dict (Int, Int) Int }

init : (Model, Cmd Msg)
init =
    ( { mines = Set.empty
      , exposed = Set.empty
      , targets = Dict.empty }
    , Random.generate InitGame (randomGame 1) )

randomPos : Random.Generator (Int, Int)
randomPos = Random.pair (Random.int 0 (size - 1)) (Random.int 0 (size - 1))

randomPoss : Int -> Set.Set (Int, Int) -> Random.Generator (Set.Set (Int, Int))
randomPoss n availablePoss =
    availablePoss
    |> Set.toList
    |> Array.fromList
    |> randomSample n
    |> Random.map (Array.toList >> Set.fromList)

randomTargets : Int -> Set.Set (Int, Int) -> Random.Generator (Dict.Dict (Int, Int) Int)
randomTargets n availablePoss =
    let rposs =
            randomPoss n availablePoss
            |> Random.map Set.toList
        rpoints =
            Random.list n (Random.int 2 3)
    in
        Random.map2 (\poss points -> listZip poss points |> Dict.fromList) rposs rpoints

randomMines : Int -> Set.Set (Int, Int) -> Random.Generator (Set.Set (Int, Int))
randomMines = randomPoss

randomGame : Int -> Random.Generator Model
randomGame level =
    let numTargets = 5 + level
        numMines = (25 - numTargets) // 2
        rMines = randomPoss numMines allPoss
        rTargetPoss = Random.map (Set.diff allPoss) rMines
        rTargets = Random.andThen rTargetPoss (randomTargets numTargets) 
    in
        Random.map2 (\ms ts -> {mines=ms, targets=ts, exposed=Set.empty}) rMines rTargets

genTargets : List (Int, Int) -> Random.Generator (Dict.Dict (Int, Int) Int)
genTargets l =
    Random.list (List.length l) (Random.int 2 3)
    |> Random.map2 listZip (randomConst l)
    |> Random.map Dict.fromList

genGame : Random.Generator Model
genGame =
    (randomPos
    |> Random.list 25
    |> Random.map (Set.fromList >> Set.toList >> listHalves)
    |> Random.map (\(l, r) -> (Set.fromList l, r)))
    `Random.andThen` (\(l, r) -> Random.pair (randomConst l) (genTargets r))
    |> Random.map (\(ms, ts) -> {mines=ms, targets=ts, exposed=Set.empty})

type Msg
    = NoOp
    | InitGame Model
    | Expose Int Int
    | NewGame

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
        , ("width", "50%")
        , ("margin", "0 25% 0 25%")
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
        , ("width", "2em")
        , ("height", "2em")
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
        , ("width", "2em")
        , ("height", "2em")
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
        , ("width", "2em")
        , ("height", "2em")
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

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        InitGame model -> (model, Cmd.none)
        Expose row column -> (expose model row column, Cmd.none)
        NewGame -> (model, Random.generate InitGame (randomGame 1))

expose : Model -> Int -> Int -> Model
expose model row column =
    { model | exposed = Set.insert (row, column) model.exposed }

-- MAIN

subscriptions : a -> Sub b
subscriptions model = Sub.none

main : Program Never
main = Html.App.program {init=init, view=view, update=update, subscriptions=subscriptions}

