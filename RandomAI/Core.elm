module RandomAI.Core exposing (..)

import Random
import Set

import Random.Extra

import AI.Core
import Game.Core exposing (Board)

init : (AI.Core.Model (), Cmd AI.Core.Msg)
init =
    let (iModel, iCmd) = AI.Core.init
    in ({ iModel | play = play }, iCmd)

play : Board -> () -> AI.Core.Action ()
play board () =
     randomRowCol board
     |> Random.generate (\(r, c) -> AI.Core.GameMsg <| Game.Core.Expose r c)
     |> AI.Core.WithEffect ()

randomRowCol : Game.Core.Board -> Random.Generator (Int, Int)
randomRowCol board =
    board.exposed
    |> Set.diff Game.Core.allPoss
    |> Set.toList
    |> Random.Extra.sample
    |> Random.map (Maybe.withDefault (0,0))