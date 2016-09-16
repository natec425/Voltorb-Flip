module RandomAI.Core exposing (..)

import Random
import Set

import Random.Extra

import AI.Core
import Game.Core

init : (AI.Core.Model, Cmd AI.Core.Msg)
init =
    let (iModel, iCmd) = AI.Core.init
    in ({ iModel | play = play }, iCmd)

play : Game.Core.Board -> (Game.Core.Model, Cmd Game.Core.Msg)
play board =
    (Game.Core.Playing board,
     randomRowCol board
     |> Random.generate (\(r, c) -> Game.Core.Expose r c))

randomRowCol : Game.Core.Board -> Random.Generator (Int, Int)
randomRowCol board =
    board.exposed
    |> Set.diff Game.Core.allPoss
    |> Set.toList
    |> Random.Extra.sample
    |> Random.map (Maybe.withDefault (0,0))