module Utils where

import GamePieces
import Color

getInPlayCoords : List GamePieces.Piece -> List (Int, Int)
getInPlayCoords pieces =
  List.map (\n -> n.coord ) (List.filter (\n -> n.inPlay) pieces)

-- Check the given list for any elements that are True.  If none are found
-- the list will be empty, which means the path is clear.
isBlocked : List Bool -> Bool
isBlocked blockChecks =
  List.isEmpty ( List.filter (\n -> n) blockChecks )

{--
  When a scout moves vertically, this will check if any pieces are in its way.
  
  - Filter the pieces list for only pieces that are in play.
  - Map that list to function that checks if the x coord is the same as the scout
  - if so, also check if the y coord is between the scout and its desitination y coord.
--}
checkYaxisBlock : Int -> Int -> Int -> List (Int, Int) -> List Bool
checkYaxisBlock x fromY toY coords =
  -- map : (a -> b) -> List a -> List b
  List.map (\n -> if (getX n) == x && (isBetween fromY toY (getY n) ) then True else False ) coords

checkXaxisBlock : Int -> Int -> Int -> List (Int, Int) -> List Bool
checkXaxisBlock y fromX toX coords =
  List.map (\n -> if (getY n) == y && (isBetween fromX toX (getX n) ) then True else False ) coords


getX (x,y) = x
getY (x,y) = y
 
isBetween : Int -> Int -> Int -> Bool
isBetween start end val =
  let
    lowerBound = min start end
    upperBound = max start end
  in
    if (val < upperBound) && (val > lowerBound) then
      True
    else
      False
     
isWithinOne : (Int, Int) -> (Int, Int) ->  Bool
isWithinOne (pieceX, pieceY) (toX, toY) =
  if | ( pieceY == (toY - 1) ) && (pieceX == toX) -> True
     | ( pieceY == (toY + 1) ) && (pieceX == toX) -> True
     | ( pieceX == (toX - 1) ) && (pieceY == toY) -> True
     | ( pieceX == (toX + 1) ) && (pieceY == toY) -> True
     | otherwise -> False

isLinearTo : (Int, Int) -> (Int, Int) -> Bool
isLinearTo (fromX, fromY) (toX, toY) =
  if | fromX == toX -> True
     | fromY == toY -> True
     | otherwise -> False
     
playerString : Color.Color -> String
playerString color =
  if color == Color.red then
    "Red"
  else
    "Blue"

-- first filter the list for pieces that are inPlay
-- then look for the one at given x,y location
-- then take the head, since there should only be one
getPieceByLocation : List GamePieces.Piece -> (Int, Int) -> Maybe GamePieces.Piece
getPieceByLocation pieces (x,y) =
  List.filter (\n -> n.inPlay) pieces
  |> List.filter (\n -> if n.coord == (x,y) then True else False)
  |> List.head