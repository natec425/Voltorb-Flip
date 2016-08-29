module Game.Core exposing (..)

import Set
import Array
import Dict
import Random
import Random.Array
import Random.Extra

-- HELPERS

listZip : List a -> List b -> List (a, b)
listZip a b =
    List.map2 (\l r -> (l, r)) a b

randomSample : Int -> Array.Array a -> Random.Generator (Array.Array a)
randomSample n l =
    l
    |> Random.Array.shuffle
    |> Random.map (Array.slice 0 n)

-- Board

size : Int
size = 5

allPoss : Set.Set (Int, Int)
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
    { mines : Set.Set (Int, Int)
    , exposed : Set.Set (Int, Int)
    , targets : Dict.Dict (Int, Int) Int }

emptyBoard : Board
emptyBoard = 
    { mines = Set.empty
    , exposed = Set.empty
    , targets = Dict.empty }

init : (Model, Cmd Msg)
init =
    ( NoGame
    , Random.generate NewBoard (randomBoard 1) )

randomPoss : Int -> Set.Set (Int, Int) -> Random.Generator (Set.Set (Int, Int))
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
            Random.map2 (\poss points -> listZip poss points |> Dict.fromList)
                        targetPoss
                        targetPoints
    in
        targets
        |> Random.map (\ts -> {b | targets=ts})

randomBoard : Int -> Random.Generator Board
randomBoard level =
    let numTargets = 5 + level
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
        NewGame -> (model, Random.generate NewBoard (randomBoard 1))

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

-- SUBSCRIPTIONS

subscriptions : a -> Sub b
subscriptions board = Sub.none


