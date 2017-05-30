module Tetris exposing (..)

import Random exposing (..)
import Time
import Html exposing (Html)
import List exposing (..)
import Keyboard exposing (..)
import TetroType exposing (..)
import Board exposing (..)

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

makeTetro : Int -> Tetromino
makeTetro i =
    case i of
        0 -> { tetro = l_type
             , current = [(0,0), (1,0), (2,0),(2,-1)]
             , position = 0
             }
        1 -> { tetro = t_type
             , current = [(0,0), (1,0), (1,-1),(2,0)]
             , position = 0
             }
        2 -> { tetro = i_type
             , current = [(0,0), (1,0), (2,0),(3,0)]
             , position = 0
             }
        3 -> { tetro = o_type
             , current = [(0,0),(1,0),(1,0),(1,1)]
             , position = 0
             }
        4 -> { tetro = s_type
             , current = [(0,0),(1,0),(1,-1),(2,-1)]
             , position = 0
             }
        5 -> { tetro = j_type
             , current = [(0,-1),(0,0),(0,1),(0,2)]
             , position = 0
             }
        6 -> { tetro = z_type
             , current = [(0,0),(1,0),(1,1),(2,1)]
             , position = 0
             }
        _ -> { tetro = l_type
             , current = [(0,0), (1,0), (2,0),(2,-1)]
             , position = 0
             }

rotateTetro: Tetromino -> Tetromino
rotateTetro t =
    let
       (cs_, p_) = rotate t.tetro t.current t.position
    in
       {tetro = t.tetro, current = cs_, position = p_}

rotate : TetroType -> List (Int,Int) -> Int -> (List (Int,Int), Int)
rotate tet cs p =
    case cs of
      [(x0,y0),(x1,y1),(x2,y2),(x3,y3)] ->
                if tet == l_type then
                        case p of
                            0 -> ([(x0+1,y0-1),
                                   (x1,  y1),
                                   (x2-1,y2+1),
                                   (x3-1,y3+2)],
                                   1)
                            1 -> ([(x0+1,y0+1),
                                   (x1,  y1),
                                   (x2-1,y2-1),
                                   (x3-2,y3)],
                                   2)
                            2 -> ([(x0-1,y0+1),
                                   (x1,  y1),
                                   (x2+1,y2-1),
                                   (x3,  y3-2)],
                                   3)
                            3 -> ([(x0-1,y0-1),
                                   (x1,  y1),
                                   (x2+1,y2+1),
                                   (x3+2,y3)],
                                   0)
                            _ -> (cs, 0)
                else if tet == t_type then
                        case p of
                            0 -> ([(x0+1,y0-1),
                                     (x1,  y1),
                                     (x2+1,y2+1),
                                     (x3-1,y3+1)],
                                     1)
                            1 -> ([(x0+1,y0+1),
                                     (x1,  y1),
                                     (x2-1,y2+1),
                                     (x3-1,y3-1)],
                                     2)
                            2 -> ([(x0-1,y0+1),
                                     (x1,  y1),
                                     (x2-1,y2-1),
                                     (x3+1,y3-1)],
                                     3)
                            3 -> ([(x0-1,y0-1),
                                     (x1,  y1),
                                     (x2+1,y2-1),
                                     (x3+1,y3+1)],
                                     0)
                            _ -> (cs, 0)
                else if tet == i_type then
                        case p of
                            0 -> ([(x0+1,y0-1),
                                     (x1+1,y1),
                                     (x2,  y2+1),
                                     (x3-1,y3+2)],
                                     1)
                            1 -> ([(x0+1,y0+2),
                                     (x1,  y1+1),
                                     (x2-1,y2),
                                     (x3-2,y3-1)],
                                     2)
                            2 -> ([(x0-2,y0+1),
                                     (x1-1,y1),
                                     (x2,  y2-1),
                                     (x3+1,y3-2)],
                                     3)
                            3 -> ([(x0-1,y0-2),
                                     (x1,  y1-1),
                                     (x2+1,y2),
                                     (x3+2,y3+1)],
                                     0)
                            _ -> (cs, 0)

                else if tet == o_type then (cs,0)

                else if tet == s_type then
                        case p of
                            0 -> ([(x0+1,y0-1),
                                     (x1,  y1),
                                     (x2+1,y2+1),
                                     (x3,  y3+2)],
                                     1)
                            1 -> ([(x0+1,y0+1),
                                     (x1,  y1),
                                     (x2-1,y2+1),
                                     (x3-1,y3)],
                                     2)
                            2 -> ([(x0-1,y0+1),
                                     (x1,  y1),
                                     (x2-1,y2-1),
                                     (x3,  y3-2)],
                                     3)
                            3 -> ([(x0-1,y0-1),
                                     (x1,  y1),
                                     (x2+1,y2-1),
                                     (x3+2,y3)],
                                     0)
                            _ -> (cs, 0)

                else if tet == j_type then
                        case p of
                            0 -> ([(x0+2,y0),
                                     (x1+1,y1-1),
                                     (x2,  y2),
                                     (x3-1,y3+1)],
                                     1)
                            1 -> ([(x0,  y0+2),
                                     (x1+1,y1+1),
                                     (x2,  y2),
                                     (x3-1,y3-1)],
                                     2)
                            2 -> ([(x0-2,y0),
                                     (x1-1,y1+1),
                                     (x2,  y2),
                                     (x3+1,y3-1)],
                                     3)
                            3 -> ([(x0,  y0-2),
                                     (x1-1,y1-1),
                                     (x2,  y2),
                                     (x3+1,y3+1)],
                                     0)
                            _ -> (cs, 0)

                else if tet == z_type then
                        case p of
                            0 -> ([(x0+2,y0),
                                     (x1+1,y1+1),
                                     (x2,  y2),
                                     (x3-1,y3-1)],
                                     1)
                            1 -> ([(x0,  y0+2),
                                     (x1-1,y1+1),
                                     (x2,  y2),
                                     (x3-1,y3-1)],
                                     2)
                            2 -> ([(x0-2,y0),
                                     (x1-1,y1-1),
                                     (x2,  y2),
                                     (x3+1,y3-1)],
                                     3)
                            3 -> ([(x0,  y0-2),
                                     (x1+1,y1-1),
                                     (x2,  y2),
                                     (x3+1,y3+1)],
                                     0)
                            _ -> (cs, 0)

                else (cs, 0)
      _ -> (cs, 0)

downTetro : Tetromino -> Tetromino
downTetro t =
    let
      cs = down t.current
    in
      {tetro = t.tetro, current = cs, position = t.position}

down : List (Int, Int) -> List (Int, Int)
down cs =
    case cs of
        [(x0,y0),(x1,y1),(x2,y2),(x3,y3)] ->
            List.map (\(x,y)-> (x,y+1)) cs
        _ -> cs

leftTetro : Tetromino -> Tetromino
leftTetro t =
    let
      cs = left t.current
    in
      {tetro = t.tetro, current = cs, position = t.position}

left : List (Int, Int) -> List (Int, Int)
left cs =
    case cs of
        [(x0,y0),(x1,y1),(x2,y2),(x3,y3)] ->
            List.map (\(x,y)-> (x-1,y)) cs
        _ -> cs

rightTetro : Tetromino -> Tetromino
rightTetro t =
    let
      cs = right t.current
    in
      {tetro = t.tetro, current = cs, position = t.position}

right : List (Int, Int) -> List (Int, Int)
right cs =
    case cs of
        [(x0,y0),(x1,y1),(x2,y2),(x3,y3)] ->
            List.map (\(x,y)-> (x+1,y)) cs
        _ -> cs

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
