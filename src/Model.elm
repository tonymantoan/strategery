-- Model definition, all the info needed to manage the state of a Strategery
-- game.

module Model where

import GamePieces exposing (..)
import Color

type alias Model =
  { pieceIsSelected : Bool
    , selectedPieceCoord : (Int, Int)
    , pieces : List Piece
    , message: String
    , stage: Int
    , turn: Color.Color
    , whoIsNext: Color.Color
    , turnComplete: Bool
    , buttonMessage: String
  }

-- game stages  
setup = 1
started = 2
over = 3

initGame : Model
initGame = 
  { pieceIsSelected = False
    , selectedPieceCoord = (0,0)
    , pieces = blues ++ reds
    , message = "Place Blue Pieces"
    , stage = setup
    , turn = Color.blue
    , whoIsNext = Color.red
    , turnComplete = False
    , buttonMessage = "Click when done"
  }