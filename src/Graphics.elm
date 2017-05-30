module Graphics exposing (..)

import Color exposing (..)
import Tetris exposing (..)
import Tetromino exposing (..)
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
    Out -> rgb 255 255 255

drawBlock : Int -> Block -> Form
drawBlock i b =
  let f = filled (blockRGB b) <| square (toFloat blockWidth)
      x = toFloat <| (blockWidth) * (i % 10) -- pixWidth // 2
      y = toFloat <| pixWidth - (blockWidth) * (i // 10)
  in
    move (x, y) f

drawTetromino : Tetromino -> List Form
drawTetromino t =
  let
    {tetro, current, position} = t
    c =
      if (tetro == l_type) then blockRGB Orange
      else if (tetro == i_type) then blockRGB Cyan
      else if (tetro == t_type) then blockRGB Purple
      else if (tetro == o_type) then blockRGB Yellow
      else if (tetro == s_type) then blockRGB Green
      else if (tetro == j_type) then blockRGB Blue
      else if (tetro == z_type) then blockRGB Red
      else blockRGB E
  in
    List.map (drawPosition c) current

drawPosition : Color -> (Int, Int) -> Form
drawPosition c (x, y) =
  let
    f = filled c <| square (toFloat blockWidth)
    xoff = toFloat <| (blockWidth) * x
    yoff = toFloat <| (blockWidth) * y - pixWidth
  in
    move (xoff, -yoff) f

toForms : Board -> List Form
toForms b = --Debug.crash "TODO"
  toList <| indexedMap drawBlock b

drawBoard : Board -> Element
drawBoard b =
  let sz = (3 * pixWidth) in
  collage sz sz (toForms b)

drawGame : Board -> Tetromino -> Element
drawGame b t =
  let sz = (3 * pixWidth) in
  collage sz sz (toForms b ++ drawTetromino t)
