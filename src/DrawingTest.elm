module DrawingTest exposing (..)

import Board exposing (..)
import Tetris exposing (..)
import TetrisGraphics exposing (..)
import Array exposing (..)
import TetroType exposing (..)
import Html exposing (Html)
import Element exposing (..)
import Time exposing (..)
import Random exposing (..)


type Msg = Tick Time

redblueboard = buildBoard altBlocks

type alias Model =
    { board : Board
    , piece : Tetromino
    , seed : Seed
    , level : Int
    }

l_start : Tetromino
l_start =
  { tetro = l_type
  , current = [(0, 0), (1, 0), (2, 0),(2,1)]
  , position = 0
  }

tetroTest : Model
tetroTest =
  { board = newBoard ,
    piece = l_start,
    seed = Random.initialSeed 0,
    level = 1
  }

view : Model -> Html msg
view model =
  toHtml <| drawGame model.board <| model.piece

init : (Model, Cmd Msg)
init = (tetroTest, Cmd.none)

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
