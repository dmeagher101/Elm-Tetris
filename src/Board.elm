module Board exposing (Block, newBoard, getBlock, setBlock)

import Array exposing (..)

width = 10
height = 20

type BlockColor =
  Cyan | Yellow | Purple | Green | Red | Blue | Orange

type Block = E | BlockColor
type alias Board = Array Block

newBoard : Board
newBoard =
  initialize (width * height) (\_ -> E)

getBlock : Board -> Int -> Int -> Block
getBlock b i j =
  case get (i * j) b of
    Just b -> b
    Nothing -> Debug.crash "Board access out of bounds"

setBlock : Block -> Board -> Int -> Int -> Board
setBlock bl bo i j =
  set (i*j) bl bo
