module MultiAI exposing (..)

import Html.App exposing (program)
import Html exposing (Html, div)

import AI.Core
import AI.View

import RandomAI.Core
import LearnAI.Core
-- MODEL

type alias Model =
    { expect : AI.Core.Model ()
    , random : AI.Core.Model ()
    , learn : AI.Core.Model LearnAI.Core.State}

-- UPDATE

type Msg
    = ExpectMsg AI.Core.Msg
    | RandomMsg AI.Core.Msg
    | LearnMsg AI.Core.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ExpectMsg msg ->
            let (aiModel, aiCmd) = AI.Core.update msg model.expect
            in ({ model | expect = aiModel }, aiCmd |> Cmd.map ExpectMsg)
        RandomMsg msg ->
            let (aiModel, aiCmd) = AI.Core.update msg model.random
            in ({ model | random = aiModel }, aiCmd |> Cmd.map RandomMsg)
        LearnMsg msg ->
            let (aiModel, aiCmd) = AI.Core.update msg model.learn
            in ({ model | learn = aiModel }, aiCmd |> Cmd.map LearnMsg)

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ AI.View.view model.expect |> Html.App.map ExpectMsg
        , AI.View.view model.random |> Html.App.map RandomMsg
        , AI.View.view model.learn |> Html.App.map LearnMsg]

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    [ model.expect |> AI.Core.subscriptions |> Sub.map ExpectMsg
    , model.random |> AI.Core.subscriptions |> Sub.map RandomMsg
    , model.learn |> AI.Core.subscriptions |> Sub.map LearnMsg ]
    |> Sub.batch

-- MAIN

init : (Model, Cmd Msg)
init =
    let (expectModel, expectCmd) = AI.Core.init
        (randomModel, randomCmd) = RandomAI.Core.init
        (learnModel, learnCmd) = LearnAI.Core.init
    in { expect = expectModel
       , random = randomModel
       , learn = learnModel }
       ! [ expectCmd |> Cmd.map ExpectMsg
         , randomCmd |> Cmd.map RandomMsg
         , learnCmd |> Cmd.map LearnMsg ]

main : Program Never
main = Html.App.program { init = init, update = update, view = view, subscriptions = subscriptions }