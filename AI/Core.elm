module AI.Core exposing (..)

import Game.Core exposing (..)

import Set
import List.Extra
import Debug exposing (crash)

-- MODEL

type alias Model = Game.Core.Model

init : ( Model, Cmd Msg )
init = Game.Core.init

action : Board -> (Int, Int)
action board =
    let response =
            allPoss
            `Set.diff` board.exposed
            |> Set.toList
            |> List.Extra.maximumBy (expectation board)
    in case response of
            Just ans -> ans
            Nothing -> crash "AI Didn't produce an answer"

expectation : Board -> (Int, Int) -> Float
expectation board (row, col) =
    if Set.member (row, col) board.exposed
    then cellValue board row col |> toFloat
    else if size - rowMines board row == 0 || size - colMines board col == 0
    then 0.0
    else min (toFloat (rowAvailablePoints board row) / toFloat( rowAvailableSpaces board row))
            (toFloat (colAvailablePoints board col) / toFloat (colAvailableSpaces board col))

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update = Game.Core.update

-- SUBSCRIPTIONS

subscriptions : a -> Sub b
subscriptions = Game.Core.subscriptions