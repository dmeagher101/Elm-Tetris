module Board exposing (..)

import Array exposing (..)
import Collage exposing (..)

width = 10
height = 20

type Block =
  E | Cyan | Yellow | Purple | Green | Red | Blue | Orange

type alias Board = Array Block

newBoard : Board
newBoard =
  buildBoard (\_ -> E)

buildBoard : (Int -> Block) -> Board
buildBoard f =
  initialize (width * height) f

boardIndex : Int -> Int -> Int
boardIndex i j =
  j * 10 + i

getBlock : Board -> Int -> Int -> Block
getBlock b i j =
  case get (boardIndex i j) b of
    Just b -> b
    Nothing -> Debug.crash "Board access out of bounds"

setBlock : Block -> Board -> Int -> Int -> Board
setBlock bl bo i j =
  set (boardIndex i j) bl bo
