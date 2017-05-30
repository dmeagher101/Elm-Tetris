module Tetris exposing (..)

import Random exposing (..)
import Time
import Html exposing (Html)
import List exposing (..)
import Keyboard exposing (..)
import TetroType exposing (..)
import Board exposing (..)
import Tetromino exposing (..)

main : Program Never Model Msg
main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
    { board : Board
    , piece : Tetromino
    , seed : Seed
    , level : Int
    , floored : Bool
    }

type Msg = Input Keyboard.KeyCode | Tick | Cycle

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

initialModel : Model
initialModel =
    { board     = newBoard
    , piece   = makeTetro 0
    , seed = Random.initialSeed 2017
    , level = 0
    , floored = False
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [
    case model.level of
        0 -> Time.every Time.second (\t -> Tick)
        1 -> Time.every (750 * Time.millisecond) (\t -> Tick)
        2 -> Time.every (500 * Time.millisecond) (\t -> Tick)
        3 -> Time.every (250 * Time.millisecond) (\t -> Tick)
        4 -> Time.every (100 * Time.millisecond) (\t -> Tick)
        _ -> Time.every (50 * Time.millisecond) (\t -> Tick) ,
    Keyboard.downs Input
  ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick -> ({ model | board = model.board
                 ,         piece = downTetro model.piece
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
                 in
                   ({ model | board = model.board
                    ,         piece = makeTetro randomInt
                    ,         seed = newSeed
                    ,         level = model.level}
                    , Cmd.none)
        --_     -> Debug.crash "Not there yet"

view : Model -> Html Msg
view model =
    Debug.crash "TODO"
