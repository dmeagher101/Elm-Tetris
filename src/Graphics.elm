module Graphics exposing (..)

import Color exposing (..)
import Tetromino exposing (..)
import Board exposing (..)
import Collage exposing (..)
import Array exposing (..)
import Element exposing (..)
import Text exposing (..)
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
    c = blockRGB <| tetroBlock t
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

nextShift : (Int, Int) -> (Int, Int)
nextShift (x,y) =
  (x+11, y+3)

drawNext : Tetromino -> (List Form)
drawNext t =
  let
    ps_ = List.map nextShift t.current
    t_ = { tetro = t.tetro, current = ps_, position = t.position}
    w = fromString "Next"
    wForm = scale 1.5 <| move (pixWidth*1.2, pixWidth ) <| text w
  in
    wForm::(drawTetromino t_)

holdShift : (Int, Int) -> (Int, Int)
holdShift (x,y) =
  (x-5, y+3)

drawHold : Maybe Tetromino -> (List Form)
drawHold h =
  case h of
    Nothing -> []
    Just t ->
      let
        ps_ = List.map holdShift t.current
        t_ = { tetro = t.tetro, current = ps_, position = t.position}
        w = fromString "Hold"
        wForm = scale 1.5 <| move (-(pixWidth*0.35), pixWidth) <| text w
      in
        wForm::drawTetromino t_

drawTitle : (List Form)
drawTitle =
  let
    w = fromString "TetrElm"
  in
    [scale 3 <| move (pixWidth * 0.5, pixWidth * 1.3) <| text w]

drawScore : Int -> (List Form)
drawScore n =
  let
    w = Text.fromString <| ("Score: " ++ toString n)
  in
    [scale 2 <| move (pixWidth, -pixWidth) <| text w]

drawGame : Board -> Tetromino -> Tetromino -> Maybe Tetromino -> Int -> Element
drawGame b t n h s =
  let sz = (3 * pixWidth) in
  collage (floor (sz * 1.8)) (floor <| sz) (toForms b ++ drawTetromino t ++
    drawNext n ++ drawHold h ++ drawTitle ++ drawScore s)
