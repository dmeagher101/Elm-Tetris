module TetrisGraphics exposing (..)

import Color exposing (..)
import Board exposing (..)
import Collage exposing (..)
import Array exposing (..)
import Element exposing (..)
--import Svg exposing (..)

pixWidth = 200
blockWidth = pixWidth // 10

blockRGB : Block -> Color
blockRGB b =
  case b of
    E -> rgb 211 211 211
    Cyan ->  rgb 0 255 255
    Yellow -> rgb 255 255 0
    Purple -> rgb 128 0 128
    Green -> rgb 0 255 0
    Red -> rgb 255 0 0
    Blue -> rgb 0 0 255
    Orange -> rgb 255 165 0

drawBlock : Int -> Block -> Form
drawBlock i b =
  let f = filled (blockRGB b) <| square (toFloat blockWidth)
      x = toFloat <| (blockWidth) * (i % 10) -- pixWidth // 2
      y = toFloat <| (blockWidth) * (i // 10) - pixWidth
  in
    move (x, -y) f

toForms : Board -> List Form
toForms b = --Debug.crash "TODO"
  toList <| indexedMap drawBlock b

drawBoard : Board -> Element
drawBoard b =
  let sz = (3 * pixWidth) in
  collage sz sz (toForms b)
