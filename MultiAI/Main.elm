module MultiAI exposing (..)

import Html.App exposing (program)
import Array exposing (Array)
import Html exposing (Html, span)

import AI.Core
import AI.View

import RandomAI.Core

-- MODEL

type alias Model = Array AI.Core.Model

-- UPDATE

type Msg
    = Msg Int AI.Core.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Msg i m ->
            model
            |> Array.get i
            |> Maybe.map (AI.Core.update m)
            |> Maybe.map (\(aiModel, aiCmd) -> (Array.set i aiModel model, aiCmd |> Cmd.map (Msg i)))
            |> Maybe.withDefault (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    span [] (model
            |> Array.indexedMap (\i ai -> Html.App.map (Msg i) (AI.View.view ai))
            |> Array.toList)

-- SUBS

subscriptions : Model -> Sub Msg
subscriptions model =
    model
    |> Array.indexedMap (\i aiModel -> AI.Core.subscriptions aiModel |> Sub.map (Msg i))
    |> Array.toList
    |> Sub.batch

-- MAIN

init : (Model, Cmd Msg)
init =
    [ AI.Core.init
    , RandomAI.Core.init ]
    |> Array.fromList
    |> Array.indexedMap (\i (aiModel, aiCmd) -> (aiModel, aiCmd |> Cmd.map (Msg i)))
    |> Array.toList
    |> List.unzip
    |> (\(ais, cmds) -> Array.fromList ais ! cmds)

main : Program Never
main = Html.App.program { init = init, update = update, view = view, subscriptions = subscriptions }