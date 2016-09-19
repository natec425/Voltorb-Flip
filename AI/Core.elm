module AI.Core exposing (..)

import Game.Core exposing (..)

import Set
import List.Extra
import Time exposing (Time, millisecond, every)
import Debug exposing (crash)

-- MODEL

type Action a
    = WithEffect (Cmd Msg)
    | WithoutEffect Int Int a

type alias Model aiState =
    { wins : Int
    , losses : Int
    , points : Int
    , gameModel : Game.Core.Model
    , playing : Bool
    , play : Board -> Action aiState
    , aiState : aiState
    , onWin : Board -> aiState -> aiState
    , onLose : Board -> aiState -> aiState }

init : ( Model (), Cmd Msg )
init =
    let (gameModel, gameCmd) = Game.Core.init
    in ({ wins = 0
        , losses = 0
        , points = 0
        , gameModel = gameModel
        , playing = False
        , play = play
        , onWin = \_ _ -> ()
        , onLose = \_ _ -> ()
        , aiState = () }
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
            Nothing -> crash "AI didn't produce an answer"

expectation : Board -> (Int, Int) -> Float
expectation board (row, col) =
    if Set.member (row, col) board.exposed
    then cellValue board row col |> toFloat
    else if size - rowMines board row == 0 || size - colMines board col == 0
    then 0.0
    else min (toFloat (rowAvailablePoints board row) / toFloat(rowAvailableSpaces board row))
            (toFloat (colAvailablePoints board col) / toFloat (colAvailableSpaces board col))

-- UPDATE
type Msg
    = Play
    | AutoPlay
    | GameMsg Game.Core.Msg

play : Board -> Action ()
play board =
    let (row, column) = action board
    in WithoutEffect row column ()

update : Msg -> Model aiState -> ( Model aiState, Cmd Msg )
update msg model =
    case (msg, model.gameModel) of
        (Play, Playing board) ->
            case model.play board of
                WithoutEffect row column newAiState ->
                    Game.Core.update (Expose row column) model.gameModel
                    |> wrapGameUpdate { model | aiState = newAiState }
                WithEffect cmd ->
                    (model, cmd)
        (Play, NoGame) ->
            update (GameMsg NewGame) model
        (Play, Won board) ->
            update (GameMsg NewGame) {model | wins = model.wins + 1
                                            , points = model.points + score board
                                            , aiState = model.onWin board model.aiState }
        (Play, Lost board) ->
            update (GameMsg NewGame) {model | losses = model.losses + 1
                                            , aiState = model.onLose board model.aiState }
        (GameMsg gameMsg, _) ->
            let (gameModel, gameCmd) = Game.Core.update gameMsg model.gameModel
            in ({model | gameModel = gameModel}, gameCmd |> Cmd.map GameMsg)
        (AutoPlay, _) ->
            ({ model | playing = not model.playing}, Cmd.none)

wrapGameUpdate : Model aiState -> (Game.Core.Model, Cmd Game.Core.Msg) -> (Model aiState, Cmd Msg)
wrapGameUpdate m (gm, gc) =
    ({ m | gameModel = gm }, gc |> Cmd.map GameMsg)

-- SUBSCRIPTIONS

subscriptions : Model aiState -> Sub Msg
subscriptions model =
    if model.playing
    then Sub.batch [ Game.Core.subscriptions model.gameModel
                   , Time.every (5 * millisecond) (\_ -> Play) ]
    else Game.Core.subscriptions model.gameModel