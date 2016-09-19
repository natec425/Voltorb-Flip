module LearnAI.Core exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Debug exposing (log)

import List.Extra

import AI.Core
import Game.Core

type alias Summaries = List (Int, Int)

type alias State = Dict Summaries (Dict (Int, Int) Int)

init : (AI.Core.Model State, Cmd AI.Core.Msg)
init =
    let (aiInit, aiCmd) = AI.Core.init
    in ({ aiInit
        | aiState = Dict.empty
        , play = play
        , onWin = updateScores
        , onLose = updateScores }
        , aiCmd)

summaries : Game.Core.Board -> Summaries
summaries board =
    List.append (rowSummaries board) (columnSummaries board)

rowSummaries : Game.Core.Board -> Summaries
rowSummaries board =
    [0, 1, 2, 3, 4]
    |> List.map (\r -> (Game.Core.rowPoints board r, Game.Core.rowMines board r))

columnSummaries : Game.Core.Board -> Summaries
columnSummaries board =
    [0, 1, 2, 3, 4]
    |> List.map (\c -> (Game.Core.colPoints board c, Game.Core.colMines board c))

play : Game.Core.Board -> State -> AI.Core.Action State
play board state =
    let currentSummaries =
            summaries board

        currentScores =
            state
            |> Dict.get currentSummaries
            |> Maybe.withDefault Dict.empty

        moveOptions =
            Game.Core.allPoss `Set.diff` board.exposed

        (row, column) =
            moveOptions
            |> Set.toList
            |> List.Extra.maximumBy (\m -> Dict.get m currentScores |> Maybe.withDefault 0)
            |> Maybe.withDefault (0, 0)

    in AI.Core.WithoutEffect row column state


updateScores : Game.Core.Board -> State -> State
updateScores board state =
    let currentSummaries =
            summaries board

        scoreChanges =
            board.exposed
            |> Set.toList
            |> List.map (\(r, c) -> ((r, c), Game.Core.cellValue board r c))
            |> Dict.fromList

        existingScores =
            state
            |> Dict.get currentSummaries
            |> Maybe.withDefault Dict.empty

        mergedScores =
            existingScores
            |> Dict.map (\k v -> v + (Dict.get k scoreChanges |> Maybe.withDefault 0))
            |> \es -> Dict.union es scoreChanges
    in state
       |> Dict.insert currentSummaries mergedScores
