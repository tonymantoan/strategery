module GamePieces where

import Color exposing (..)

-- Definition of a piece record.
type alias Piece =
  { id : Int
    , coord : (Int, Int)
    , value : Int
    , color : Color
    , inPlay : Bool
  }
  
-- TODO: automatically increment the pieceId
-- Constructor for a piece record.
makePiece : Int -> (Int, Int) -> Int -> Color -> Piece
makePiece pid location val col =
  { id = pid
    ,coord = location
    , value = val
    , color = col
    , inPlay = True
  }
  
-- Definitions for values of pieces that don't have a numeric value in Statego
spy = -1
bomb = 0
flag = 11

-- Create a lists of all blue and red pieces.
blues = [ (makePiece 1 (0,0) 10 blue), (makePiece 2 (1,0) 9 blue), (makePiece 3 (2,0) 8 blue), (makePiece 4 (3,0) 8 blue),
          (makePiece 5 (4,0) 7 blue), (makePiece 6 (5,0) 7 blue), (makePiece 7 (6,0) 7 blue),
          (makePiece 8 (7,0) 6 blue), (makePiece 9 (8,0) 6 blue), (makePiece 10 (9,0) 6 blue), (makePiece 11 (0,1) 6 blue),
          (makePiece 12 (1,1) 5 blue), (makePiece 13 (2,1) 5 blue), (makePiece 14 (3,1) 5 blue), (makePiece 15 (4,1) 5 blue),
          (makePiece 16 (5,1) 4 blue), (makePiece 17 (6,1) 4 blue), (makePiece 18 (7,1) 4 blue), (makePiece 19 (8,1) 4 blue),
          (makePiece 20 (9,1) 3 blue), (makePiece 21 (0,2) 3 blue), (makePiece 22 (1,2) 3 blue), (makePiece 23 (2,2) 3 blue),
          (makePiece 24 (3,2) 3 blue), (makePiece 25 (4,2) 2 blue), (makePiece 26 (5,2) 2 blue), (makePiece 27 (6,2) 2 blue),
          (makePiece 28 (7,2) 2 blue), (makePiece 29 (8,2) 2 blue), (makePiece 30 (9,2) 2 blue), (makePiece 31 (0,3) 2 blue),
          (makePiece 32 (1,3) 2 blue), (makePiece 33 (2,3) -1 blue), (makePiece 34 (3,3) 0 blue), (makePiece 35 (4,3) 0 blue),
          (makePiece 36 (5,3) 0 blue), (makePiece 37 (6,3) 0 blue), (makePiece 38 (7,3) 0 blue), (makePiece 39 (8,3) 0 blue),
          (makePiece 40 (9,3) 11 blue)
        ]

reds = [ (makePiece 41 (0,6) 10 red), (makePiece 42 (1,6) 9 red), (makePiece 43 (2,6) 8 red), (makePiece 44 (3,6) 8 red),
          (makePiece 45 (4,6) 7 red), (makePiece 46 (5,6) 7 red), (makePiece 47 (6,6) 7 red),
          (makePiece 48 (7,6) 6 red), (makePiece 49 (8,6) 6 red), (makePiece 50 (9,6) 6 red), (makePiece 51 (0,7) 6 red),
          (makePiece 52 (1,7) 5 red), (makePiece 53 (2,7) 5 red), (makePiece 54 (3,7) 5 red), (makePiece 55 (4,7) 5 red),
          (makePiece 56 (5,7) 4 red), (makePiece 57 (6,7) 4 red), (makePiece 58 (7,7) 4 red), (makePiece 59 (8,7) 4 red),
          (makePiece 60 (9,7) 3 red), (makePiece 61 (0,8) 3 red), (makePiece 62 (1,8) 3 red), (makePiece 63 (2,8) 3 red),
          (makePiece 64 (3,8) 3 red), (makePiece 65 (4,8) 2 red), (makePiece 66 (5,8) 2 red), (makePiece 67 (6,8) 2 red),
          (makePiece 68 (7,8) 2 red), (makePiece 69 (8,8) 2 red), (makePiece 70 (9,8) 2 red), (makePiece 71 (0,9) 2 red),
          (makePiece 72 (1,9) 2 red), (makePiece 73 (2,9) -1 red), (makePiece 74 (3,9) 0 red), (makePiece 75 (4,9) 0 red),
          (makePiece 76 (5,9) 0 red), (makePiece 77 (6,9) 0 red), (makePiece 78 (7,9) 0 red), (makePiece 79 (8,9) 0 red),
          (makePiece 80 (9,9) 11 red)
        ]