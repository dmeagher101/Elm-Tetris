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

redblueboard = buildBoard altRows

type alias Model =
    { board : Board
    , piece : Tetromino
    , seed : Seed
    , level : Int
    }

l_start : Tetromino
l_start =
  { tetro = l_type
  , current = [(0, 0), (1, 0), (2, 0), (2,1)]
  , position = 0
  }

blank : Tetromino
blank =
  { tetro = l_type
  , current = []
  , position = 0
  }

initialmodel : Model
initialmodel =
  { board = redblueboard ,
    piece = blank,
    seed = Random.initialSeed 0,
    level = 1
  }

view : Model -> Html msg
view model =
  toHtml <| drawGame model.board <| model.piece

init : (Model, Cmd Msg)
init = (initialmodel, Cmd.none)

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
  every (1 * second) Tick

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newmodel = {
      seed = model.seed,
      piece = model.piece,
      board = clearLine 19 model.board,
      level = 1
  }
  in
    case msg of
      Tick t -> (newmodel, Cmd.none)

main : Program Never Model Msg
main =
  Html.program
  {
    init = init,
    subscriptions = subscriptions,
    view = view,
    update = update
  }
