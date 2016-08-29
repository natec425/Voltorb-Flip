module AI.Core exposing (..)

import Game.Core exposing (..)

import Set
import List.Extra
import Debug exposing (crash, log)

-- HELPERS

cmd : a -> Cmd a
cmd a = Cmd.none |> Cmd.map (\_ -> a)

-- MODEL

type alias Model = Game.Core.Model

wrapMC : ( a, Cmd Game.Core.Msg ) -> ( a, Cmd Msg )
wrapMC (m, c) = (m, c |> Cmd.map GameMsg)

init : ( Model, Cmd Msg )
init = Game.Core.init
       |> wrapMC
            

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
type Msg
    = Play
    | GameMsg Game.Core.Msg

play : Board -> (Model, Cmd Msg)
play board =
    action board
    |> uncurry Expose
    |> \msg -> Game.Core.update msg (Playing board)
    |> wrapMC

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case log "update" (msg, model) of
        (Play, Playing board) ->
            play board
        (Play, _) ->
            (model, Cmd.none)
        (GameMsg gameMsg, _) ->
            Game.Core.update gameMsg model
            |> wrapMC


-- SUBSCRIPTIONS

subscriptions : a -> Sub b
subscriptions = Game.Core.subscriptions