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
    Random.Array.shuffle l
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

type alias Board =
    { mines : Set.Set (Int, Int)
    , exposed : Set.Set (Int, Int)
    , targets : Dict.Dict (Int, Int) Int }

emptyGame : Board
emptyGame = 
    { mines = Set.empty
    , exposed = Set.empty
    , targets = Dict.empty }

init : (Board, Cmd Msg)
init =
    ( { mines = Set.empty
      , exposed = Set.empty
      , targets = Dict.empty }
    , Random.generate InitGame (randomGame 1) )

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
    let availablePoss = Set.diff allPoss b.mines
        targetPoss = randomPoss n availablePoss |> Random.map (Set.toList)
        targetPoints = Random.list n (Random.int 2 3)
        targets = Random.map2 (\poss points -> listZip poss points |> Dict.fromList)
                              targetPoss
                              targetPoints
    in
        Random.map (\ts -> {b | targets=ts}) targets

randomGame : Int -> Random.Generator Board
randomGame level =
    let numTargets = 5 + level
        numMines = (25 - numTargets) // 2
    in
        Random.Extra.constant emptyGame
        `Random.andThen` (populateRandomMines numMines)
        `Random.andThen` (populateRandomTargets numTargets)

type Msg
    = NoOp
    | InitGame Board
    | Expose Int Int
    | NewGame

-- UPDATE

update : Msg -> Board -> (Board, Cmd Msg)
update msg board =
    case msg of
        NoOp -> (board, Cmd.none)
        InitGame board -> (board, Cmd.none)
        Expose row column -> (expose board row column, Cmd.none)
        NewGame -> (board, Random.generate InitGame (randomGame 1))

expose : Board -> Int -> Int -> Board
expose board row column =
    { board | exposed = Set.insert (row, column) board.exposed }

-- SUBSCRIPTIONS

subscriptions : a -> Sub b
subscriptions board = Sub.none


