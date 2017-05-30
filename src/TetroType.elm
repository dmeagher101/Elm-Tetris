module TetroType exposing (..)

import Array exposing (..)
import Board exposing (..)

type alias TetroType = Array (Int, Int)

type alias Tetromino =
    { tetro : TetroType
    , current : List (Int, Int)
    , position : Int
    }

l_type : TetroType
l_type =
  fromList [(0,0), (0,1), (0,2), (1,2)]

t_type : TetroType
t_type =
  fromList [(0, 0), (1, 0), (2, 0), (1, 1)]

i_type : TetroType
i_type =
  fromList [(0,0), (1,0), (2, 0), (3, 0)]

o_type : TetroType
o_type =
  fromList [(0,0), (1,0), (0,1), (1,1)]

s_type : TetroType
s_type =
  fromList [(1,0), (2,0), (0,1), (1,1)]

j_type : TetroType
j_type =
  fromList [(1,0), (1,1), (0,2), (1,2)]

z_type : TetroType
z_type =
  fromList [(0,0), (1,0), (0,1), (1,1)]
