module Test exposing (..)

import Board exposing (..)
import Tetris exposing (..)
import Graphics exposing (..)
import Array exposing (..)
--import TetroType exposing (..)
import Html exposing (Html)
import Element exposing (..)
import Time exposing (..)
import Random exposing (..)
import Keyboard exposing (..)
import Tetromino exposing (..)

type Msg = Tick | Input KeyCode | Cycle | Over | Check

redblueboard = buildBoard altRows

type alias Model =
    {board : Board
    , piece : Tetromino
    , hold : Maybe Tetromino
    , next : Tetromino
    , seed : Seed
    , level : Int
    , floored : Bool
    , over : Bool
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
  { board = newBoard ,
    piece = makeTetro 6,
    hold = Just <| makeTetro 3,
    next = makeTetro 2,
    seed = Random.initialSeed 0,
    level = 1,
    floored = False,
    over = False
  }

view : Model -> Html msg
view model =
  toHtml <| (drawGame model.board model.piece model.next model.hold)

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
  Sub.batch
  [
    every (1 * millisecond) (\t -> if (model.floored) then Cycle
                                     else if (model.over) then Over
                                     else Check),
    every (1 * second) (\t -> Tick),
    Keyboard.downs Input
  ]

{--update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newmodel = {
      seed = model.seed,
      piece = model.piece,
      board = clearLine 10 model.board,
      level = 1
  }
  in
    case msg of
      Tick t -> (newmodel, Cmd.none)--}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Check ->
          let
            oldPiece = model.piece
          in
            if (checkBelow model.board oldPiece.current)
            then
                ({ model | board = setTetro model.board (downTetro oldPiece)
                 ,         piece = oldPiece
                 ,         seed = model.seed
                 ,         level = model.level
                 ,         floored = True
                 }
                 , Cmd.none)
            else
                ({ model | board = model.board
                 ,         piece = oldPiece
                 ,         seed = model.seed
                 ,         level = model.level
                 }
                 , Cmd.none)
        Tick ->
          let
            oldPiece = model.piece
            newPiece = downTetro model.piece
          in
            if (checkBelow model.board oldPiece.current)
            then
                ({ model | board = setTetro model.board oldPiece
                 ,         piece = model.piece
                 ,         seed = model.seed
                 ,         level = model.level
                 ,         floored = True
                 }
                 , Cmd.none)
            else
                ({ model | board = model.board
                 ,         piece = newPiece
                 ,         seed = model.seed
                 ,         level = model.level
                 }
                 , Cmd.none)
        Input 37 -> ({ model | board = model.board
                 ,         piece = leftTetro model.piece
                 ,         seed = model.seed
                 ,         level = model.level
                 }
                 , Cmd.none)
        Input 38 -> ({ model | board = model.board
                 ,         piece = rotateTetro model.piece
                 ,         seed = model.seed
                 ,         level = model.level
                 }
                 , Cmd.none)
        Input 39 -> ({ model | board = model.board
                 ,         piece = rightTetro model.piece
                 ,         seed = model.seed
                 ,         level = model.level
                 }
                 , Cmd.none)
        Input 40 -> ({ model | board = model.board
                 ,         piece = downTetro model.piece
                 ,         seed = model.seed
                 ,         level = model.level
                 }
                 , Cmd.none)
        Input _  -> (model, Cmd.none)
        Cycle -> let
                   (randomInt, newSeed) = Random.step (int 0 6) model.seed
                   nextPiece = model.next
                   newPiece = makeTetro randomInt
                 in
                 if (checkBelow model.board nextPiece.current)
                 then
                    ({ model | board = setTetro model.board newPiece
                     ,         piece = model.piece
                     ,         over = True
                     }
                     , Cmd.none)
                  else
                   ({ model | board = model.board
                    ,         piece = model.next
                    ,         next = newPiece
                    ,         seed = newSeed
                    ,         level = model.level
                    ,         floored = False
                    }
                    , Cmd.none)
        Over -> ({ model | board = model.board
                    ,         level = model.level
                    ,         floored = False
                    }
                  , Cmd.none)
        --_     -> Debug.crash "Not there yet"


main : Program Never Model Msg
main =
  Html.program
  {
    init = init,
    subscriptions = subscriptions,
    view = view,
    update = update
  }
