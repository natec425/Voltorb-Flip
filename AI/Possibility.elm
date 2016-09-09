module AI.Possibility exposing (..)

import Set exposing (Set)
import Set
import Dict exposing (Dict)
import Dict
import Array exposing (Array)
import Array

type Cell
    = Point Int
    | Mine

one = Point 1
two = Point 2
three = Point 3

type alias Board = Array (Array Cell)

emptyBoard = Array.initialize 5 (\_ -> Array.initialize 5 (\_ -> Mine))

type alias RowSummary =
    { points : Int
    , mines : Int }

type alias ColumnSummary = RowSummary

type alias State =
    { cellSummary : Dict (Int, Int) (RowSummary, ColumnSummary)
    , exposed : Dict (Int, Int) Cell }

possibilities : State -> Set Board
possibilities state =
    let iter row col board =
            let itern =
                    iter (nextRow row) (nextCol col)
            in if row > 4 || col > 4
               then Set.singleton board
               else (with one row col board |> itern)
                   `Set.union` (with two row col board |> itern)
                   `Set.union` (with three row col board |> itern)
                   `Set.union` (with Mine row col board |> itern)
    in iter 0 0 emptyBoard

nextRow : Int -> Int
nextRow row = (row + 1) % 5

nextCol : Int -> Int
nextCol = nextRow

with : Cell -> Int -> Int -> State -> Board -> Set Board
with cell row col state board =
    case cell of
        Point n ->  
        Mine ->

valid : State -> Board -> Bool
valid state board = true