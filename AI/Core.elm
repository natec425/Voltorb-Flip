module AI.Core exposing (..)

import Game.Core exposing (..)

import Set
import List.Extra
import Debug exposing (crash, log)
import Time exposing (Time, millisecond, every)

-- HELPERS

cmd : a -> Cmd a
cmd a = Cmd.none |> Cmd.map (\_ -> a |> log "cmd")

batchCmd : ( a, Cmd b ) -> Cmd b -> ( a, Cmd b )
batchCmd (m, c1) c2 =
    (m, Cmd.batch [c1, c2])
-- MODEL

type alias Model =
    { wins : Int
    , losses : Int
    , gameModel : Game.Core.Model }

wrapMC : ( a, Cmd Game.Core.Msg ) -> ( a, Cmd Msg )
wrapMC (m, c) = (m, c |> Cmd.map GameMsg)

init : ( Model, Cmd Msg )
init =
    let (gameModel, gameCmd) = Game.Core.init
    in ({ wins = 0
        , losses = 0
        , gameModel = gameModel }
       , gameCmd |> Cmd.map GameMsg)
            

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
    | AutoPlay
    | GameMsg Game.Core.Msg

play : Board -> (Game.Core.Model, Cmd Msg)
play board =
    action board
    |> uncurry Expose
    |> \msg -> Game.Core.update msg (Playing board)
    |> wrapMC

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case log "update" (msg, model.gameModel) of
        (Play, Playing board) ->
            let (gameModel, cmd) = play board
            in ({model | gameModel = gameModel}, cmd)
        (Play, _) ->
            (model, Cmd.none)
        (AutoPlay, Playing board) ->
            update Play model
        (AutoPlay, NoGame) ->
            update (GameMsg NewGame) model
        (AutoPlay, Won _) ->
            update (GameMsg NewGame) {model | wins = model.wins + 1}
        (AutoPlay, Lost _) ->
            update (GameMsg NewGame) {model | losses = model.losses + 1}
        (GameMsg gameMsg, _) ->
            let (gameModel, gameCmd) = Game.Core.update gameMsg model.gameModel
            in ({model | gameModel = gameModel}, gameCmd |> Cmd.map GameMsg)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch [Game.Core.subscriptions model.gameModel,
               Time.every (50 * millisecond) (\_ -> AutoPlay) ]