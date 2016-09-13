module Game.Core exposing (..)

import Set exposing (Set)
import Array exposing (Array)
import Dict exposing (Dict)
import Random
import Random.Array

import List.Extra
import Random.Extra

-- HELPERS

randomSample : Int -> Array a -> Random.Generator (Array a)
randomSample n l =
    l
    |> Random.Array.shuffle
    |> Random.map (Array.slice 0 n)

-- Board

size : Int
size = 5

allPoss : Set (Int, Int)
allPoss =
    [ (0, 0), (1, 0), (2, 0), (3, 0), (4, 0)
    , (0, 1), (1, 1), (2, 1), (3, 1), (4, 1)
    , (0, 2), (1, 2), (2, 2), (3, 2), (4, 2)
    , (0, 3), (1, 3), (2, 3), (3, 3), (4, 3)
    , (0, 4), (1, 4), (2, 4), (3, 4), (4, 4)]
    |> Set.fromList

type Model
    = NoGame
    | Playing Board
    | Won Board
    | Lost Board

type alias Board =
    { mines : Set (Int, Int)
    , exposed : Set (Int, Int)
    , targets : Dict (Int, Int) Int }

emptyBoard : Board
emptyBoard =
    { mines = Set.empty
    , exposed = Set.empty
    , targets = Dict.empty }

init : (Model, Cmd Msg)
init =
    ( NoGame
    , Random.generate NewBoard randomBoard )

randomPoss : Int -> Set (Int, Int) -> Random.Generator (Set (Int, Int))
randomPoss n availablePoss =
    availablePoss
    |> Set.toList
    |> Array.fromList
    |> randomSample n
    |> Random.map (Array.toList >> Set.fromList)

populateRandomMines : Int -> Board -> Random.Generator Board
populateRandomMines n b =
    randomPoss n allPoss
    |> Random.map (\ms -> {b | mines=ms })

populateRandomTargets : Int -> Board -> Random.Generator Board
populateRandomTargets n b =
    let availablePoss =
            Set.diff allPoss b.mines

        targetPoss =
            availablePoss
            |> randomPoss n
            |> Random.map (Set.toList)

        targetPoints =
            Random.int 2 3
            |> Random.list n

        targets =
            Random.map2 (\poss points -> List.Extra.zip poss points |> Dict.fromList)
                        targetPoss
                        targetPoints
    in
        targets
        |> Random.map (\ts -> {b | targets=ts})

randomBoard : Random.Generator Board
randomBoard =
    let numTargets = 6
        numMines = (25 - numTargets) // 2
    in
        Random.Extra.constant emptyBoard
        `Random.andThen` (populateRandomMines numMines)
        `Random.andThen` (populateRandomTargets numTargets)

-- UPDATE

type Msg
    = NoOp
    | NewBoard Board
    | Expose Int Int
    | NewGame

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        NewBoard board -> (Playing board, Cmd.none)
        Expose row column -> (expose model row column, Cmd.none)
        NewGame -> (model, Random.generate NewBoard randomBoard)

expose : Model -> Int -> Int -> Model
expose model row column =
    case model of
        NoGame -> NoGame
        Playing board -> exposePlaying board row column
        Won board -> Won board
        Lost board -> Lost board

exposePlaying : Board -> Int -> Int -> Model
exposePlaying board row column =
    let board' =
            { board | exposed = Set.insert (row, column) board.exposed }
    in if hasExposedMine board' then Lost board'
       else if allTargetsExposed board' then Won board'
       else Playing board'

hasExposedMine : Board -> Bool
hasExposedMine board =
    Set.intersect board.exposed board.mines
    |> Set.isEmpty
    |> not

allTargetsExposed : Board -> Bool
allTargetsExposed board =
    let targetPoss =
            board.targets
            |> Dict.keys
            |> Set.fromList
    in Set.diff targetPoss board.exposed
       |> Set.isEmpty

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

score : Board -> Int
score board =
    if Set.isEmpty board.exposed
    then 0
    else board.exposed
        |> Set.toList
        |> List.map (\p -> Dict.get p board.targets |> Maybe.withDefault 1)
        |> List.product

cellValue : Board -> Int -> Int -> Int
cellValue board row column =
    if Set.member (row, column) board.mines
    then 0
    else case Dict.get (row, column) board.targets of
            Just v -> v
            Nothing -> 1

exposedRowPoints : Board -> Int -> Int
exposedRowPoints board row =
    [0, 1, 2, 3, 4]
    |> List.filter (\c -> not (Set.member (row, c) board.mines))
    |> List.filter (\c -> Set.member (row, c) board.exposed)
    |> List.map (\c -> Dict.get (row, c) board.targets)
    |> List.map (Maybe.withDefault 1)
    |> List.sum

exposedColPoints : Board -> Int -> Int
exposedColPoints board col =
    [0, 1, 2, 3, 4]
    |> List.filter (\r -> not (Set.member (r, col) board.mines))
    |> List.filter (\r -> Set.member (r, col) board.exposed)
    |> List.map (\r -> Dict.get (r, col) board.targets)
    |> List.map (Maybe.withDefault 1)
    |> List.sum

rowAvailablePoints : Board -> Int -> Int
rowAvailablePoints board row =
    rowPoints board row - exposedRowPoints board row

rowAvailableSpaces : Board -> Int -> Int
rowAvailableSpaces board row =
    [0, 1, 2, 3, 4]
    |> List.filter (\c -> not (Set.member (row, c) board.exposed))
    |> List.length

colAvailablePoints : Board -> Int -> Int
colAvailablePoints board col =
    colPoints board col - exposedColPoints board col

colAvailableSpaces : Board -> Int -> Int
colAvailableSpaces board col =
    [0, 1, 2, 3, 4]
    |> List.filter (\r -> not (Set.member (r, col) board.exposed))
    |> List.length

-- SUBSCRIPTIONS

subscriptions : a -> Sub b
subscriptions board = Sub.none


