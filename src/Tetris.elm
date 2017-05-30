module Tetris exposing (..)

import Random exposing (..)
import Time
import Html exposing (Html)
import List exposing (..)

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

type Msg = Input | Tick | Cycle

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

initialModel : Model
initialModel =
    { board     = newBoard
    , piece   = tetroGenerator
    , seed = Random.initialSeed 2017
    , level = 0
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.level of
        0 -> Time.every Time.second (\t -> Tick)
        1 -> Time.every (750 * Time.millisecond) (\t -> Tick)
        2 -> Time.every (500 * Time.millisecond) (\t -> Tick)
        3 -> Time.every (250 * Time.millisecond) (\t -> Tick)
        4 -> Time.every (100 * Time.millisecond) (\t -> Tick)
        _ -> Time.every (50 * Time.millisecond) (\t -> Tick)

tetroGenerator : Tetromino
tetroGenerator =
    let
        (random, msg) = Random.step (Random.int 0 6) (Random.initialSeed 2017)
    in
    case random of
        0 -> { tetro = l_type
             , current = [(0, 21), (1, 21), (2, 21),(2,22)]
             , position = 0
             }
        _ -> { tetro = l_type
             , current = [(0, 21), (1, 21), (2,21),(2,22)]
             , position = 0
             }
    {-
        1 -> { tetro = t_type
             , current = [{0,21), {1,21}, {2,22},{3,21}]
             , position = 0
             }
        2 -> { tetro = i_type
             , current = [{0,21}, {0,22}, {0,23},{0,24}]
             , position = 0
             }
        3 -> { tetro = o_type
             , current = [{0,21}, {0,22}, {1,21},{1,22}]
             , position = 0
             }
        4 -> { tetro = s_type
             , current = [{0,21}, {1,21}, {1,22},{2,22}]
             , position = 0
             }
        5 -> { tetro = j_type
             , current = [{0,21}, {0,22}, {1,21},{2,21}]
             , position = 0
             }
        6 -> { tetro = z_type
             , current = [{0,21},{1,21},{1,22},{2,22}]
             , position = 0
             } -}
{-
down : Tetromino -> Tetromino
down t =
    case t of
        {t, s, c, p} -> {tetro = t
                        , spawn = s
                        , current = (List.map (\(x,y) -> (x,y-1)) c)
                        , position = p
                        }
-}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick -> ({ model | board = model.board
                 ,         piece = model.piece
                 ,         seed = model.seed
                 ,         level = model.level
                 }
                 , Cmd.none)
        _    -> Debug.crash "Not there yet"

view : Model -> Html Msg
view model =
    Debug.crash "TODO"
