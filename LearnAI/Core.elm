module LearnAI.Core exposing (..)

import AI.Core
import Game.Core exposing (..)

import Set
import Dict exposing (Dict)
import List.Extra
import Time exposing (Time, millisecond, every)
import Debug exposing (crash)

-- MODEL

type alias Model =
    { AI.Core.Model | paths : Dict Int Int }

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