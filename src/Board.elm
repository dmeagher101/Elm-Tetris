module Board exposing (..)

import Array exposing (..)
import Collage exposing (..)

width = 10
height = 20

type Block =
  E | Cyan | Yellow | Purple | Green | Red | Blue | Orange | Out

type alias Board = Array Block

newBoard : Board
newBoard =
  repeat (10*20) E

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
    Nothing -> Out

lineStart : Int -> Int
lineStart i =
  boardIndex 0 i

checkEmpty : Block -> Bool
checkEmpty b =
  case b of
    E -> True
    _ -> False

checkFull : Board -> Int -> Bool
checkFull b i =
  let ri = lineStart i
      l = filter checkEmpty <| slice ri (ri + 9) b in
  isEmpty l

lineEnd i =
  lineStart i + 9

clearLine : Int -> Board -> Board
clearLine i b =
  let end = (lineStart i)
      new = lineStart (i + 1)
      emptyline = repeat 10 E
      top = slice 0 end b
      bottom =
        if (i >= 19) then empty
        else slice new (height * width) b
  in
    append emptyline (append top bottom)

checkClear : Board -> List Int
checkClear b = 
  List.map (\x -> (if checkFull b x then x else -1)) (List.range 0 19)

clearAll : Board -> List Int -> Board
clearAll b ls =
  case ls of 
    []     -> b
    l::ls_ -> if (l>=0) 
              then clearAll (clearLine l b) ls_ 
              else clearAll b ls_  


setBlock : Block -> Board -> Int -> Int -> Board
setBlock bl bo i j =
  set (boardIndex i j) bl bo
