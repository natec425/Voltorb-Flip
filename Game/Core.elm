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

-- MODEL

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

type alias Model =
    { mines : Set.Set (Int, Int)
    , exposed : Set.Set (Int, Int)
    , targets : Dict.Dict (Int, Int) Int }

emptyGame : Model
emptyGame = 
    { mines = Set.empty
    , exposed = Set.empty
    , targets = Dict.empty }

init : (Model, Cmd Msg)
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

populateRandomMines : Int -> Model -> Random.Generator Model
populateRandomMines n m =
    randomPoss n allPoss
    |> Random.map (\ms -> {m | mines=ms })

populateRandomTargets : Int -> Model -> Random.Generator Model
populateRandomTargets n m =
    let availablePoss = Set.diff allPoss m.mines
        targetPoss = randomPoss n availablePoss |> Random.map (Set.toList)
        targetPoints = Random.list n (Random.int 2 3)
        targets = Random.map2 (\poss points -> listZip poss points |> Dict.fromList)
                              targetPoss
                              targetPoints
    in
        Random.map (\ts -> {m | targets=ts}) targets

randomGame : Int -> Random.Generator Model
randomGame level =
    let numTargets = 5 + level
        numMines = (25 - numTargets) // 2
    in
        Random.Extra.constant emptyGame
        `Random.andThen` (populateRandomMines numMines)
        `Random.andThen` (populateRandomTargets numTargets)

type Msg
    = NoOp
    | InitGame Model
    | Expose Int Int
    | NewGame

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

-- SUBSCRIPTIONS

subscriptions : a -> Sub b
subscriptions model = Sub.none


