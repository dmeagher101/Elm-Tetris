module DrawingTest exposing (..)

import Board exposing (..)
import Tetris exposing (..)
import TetrisGraphics exposing (..)
import Array exposing (..)
import Html exposing (Html)
import Element exposing (..)
import Time exposing (..)

type Msg = Tick Time

redblueboard = buildBoard altBlocks

view : Model -> Html msg
view model =
  toHtml <| drawBoard model

init : (Model, Cmd Msg)
init = ((setBlock Blue newBoard 9 19), Cmd.none)

altRows : Int -> Block
altRows i =
  if ((i // 10) % 2 == 0) then
    Blue
  else
    Red

altBlocks : Int -> Block
altBlocks i =
  if (i % 2 == 0) then
    Blue
  else
    Red

subscriptions : Model -> Sub Msg
subscriptions model =
  every (1000 * second) Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick t -> (model, Cmd.none)

main : Program Never Model Msg
main =
  Html.program
  {
    init = init,
    subscriptions = subscriptions,
    view = view,
    update = update
  }
