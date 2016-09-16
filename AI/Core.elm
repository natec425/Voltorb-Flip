module AI.Core exposing (..)

import Game.Core exposing (..)

import Set
import List.Extra
import Time exposing (Time, millisecond, every)
import Debug exposing (crash)

-- MODEL

type alias Action = Board -> (Int, Int)

type alias Model =
    { wins : Int
    , losses : Int
    , points : Int
    , gameModel : Game.Core.Model
    , playing : Bool
    , play : Board -> (Game.Core.Model, Cmd Game.Core.Msg) }

init : ( Model, Cmd Msg )
init =
    let (gameModel, gameCmd) = Game.Core.init
    in ({ wins = 0
        , losses = 0
        , points = 0
        , gameModel = gameModel
        , playing = False
        , play = play }
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

play : Board -> (Game.Core.Model, Cmd Game.Core.Msg)
play board =
    let (row, column) = action board
    in Game.Core.update (Game.Core.Expose row column) (Playing board)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model.gameModel) of
        (Play, Playing board) ->
            model.play board
            |> wrapGameUpdate model
        (Play, NoGame) ->
            update (GameMsg NewGame) model
        (Play, Won board) ->
            update (GameMsg NewGame) {model | wins = model.wins + 1
                                            , points = model.points + score board}
        (Play, Lost _) ->
            update (GameMsg NewGame) {model | losses = model.losses + 1}
        (GameMsg gameMsg, _) ->
            let (gameModel, gameCmd) = Game.Core.update gameMsg model.gameModel
            in ({model | gameModel = gameModel}, gameCmd |> Cmd.map GameMsg)
        (AutoPlay, _) ->
            ({ model | playing = not model.playing}, Cmd.none)

wrapGameUpdate : Model -> (Game.Core.Model, Cmd Game.Core.Msg) -> (Model, Cmd Msg)
wrapGameUpdate m (gm, gc) =
    ({ m | gameModel = gm }, gc |> Cmd.map GameMsg)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.playing
    then Sub.batch [ Game.Core.subscriptions model.gameModel
                   , Time.every (5 * millisecond) (\_ -> Play) ]
    else Game.Core.subscriptions model.gameModel