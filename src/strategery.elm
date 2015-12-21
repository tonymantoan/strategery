import Color exposing (..)
import Graphics.Collage
import Graphics.Element exposing (..)
import Array exposing (..)
import Mouse
import Text

import GamePieces exposing (..)
import Board exposing (..)

type alias Model =
  { pieceIsSelected : Bool
    , selectedPieceCoord : (Int, Int)
    , pieces : List Piece
    , message: String
  }

initBoard : Model
initBoard = 
  { pieceIsSelected = False
    , selectedPieceCoord = (0,0)
    , pieces = blues ++ reds
    , message = "Start Game!"
  }

-- foldp func(anyType, stateObj, stateObj) state signal
-- create a signal of Models with foldp, which is maped to the view function
main =
  Signal.map view (Signal.foldp update initBoard (Signal.sampleOn Mouse.clicks Mouse.position) )

-- TODO: Is that nested case the only way to do this?  
update : (Int,Int) -> Model -> Model
update mousePosition  model =
  if model.pieceIsSelected == False then
    -- if no piece is selected: get the clicked piece and make that the selection, return the model as is
    markSelected  (spaceClicked mousePosition) model
    |> updatePieceSelectedMessage
  else
    -- if a piece is already selected: get the clicked piece this time, mark the other space as 0, make this space = the higher of the two
    let selected = (spaceClicked mousePosition)
        attacker = getPieceByLocation model.pieces model.selectedPieceCoord 
        defender = getPieceByLocation model.pieces selected
    in
        case attacker of
          Nothing -> model
          
          Just n ->
            if ( isMoveValid n.coord selected ) == True then
              handleMove n defender selected model
            else
              resetPieceSelected {model | message <- "Invalid move"}
                
handleMove : Piece -> Maybe Piece -> (Int, Int) -> Model -> Model
handleMove attacker defender moveTo model =
  case defender of
    Nothing ->
    updatePieces {attacker | coord <- moveTo} model
    |> resetPieceSelected
              
    Just m ->
      attack attacker m model 
      |> resetPieceSelected

{--
  # View
  The collage contains the game board itself.  Other elements show
  other game related info.
--}
view : Model -> Element    
view model = flow down [
  Graphics.Collage.collage boardWidth boardHeight
      ( 
        (drawCols [0..columns]) ++ 
        (drawRows [0..rows]) ++
        (placePieces model.pieces)
      ),
      gameMessage model.message
    ]

updatePieces :  Piece -> Model -> Model
updatePieces piece model =
  -- Make a new list with the old piece filtered out, then append the new piece
  {model | pieces <- [piece] ++ (List.filter (\n -> if n.id == piece.id then False else True) model.pieces)}

resetPieceSelected : Model -> Model
resetPieceSelected model =
  { model | pieceIsSelected <- False }
  
updatePieceSelectedMessage : Model -> Model
updatePieceSelectedMessage model = {model | message <- "Selected " ++ ( toString model.selectedPieceCoord ) }
  
isMoveValid : (Int, Int) -> (Int, Int) ->  Bool
isMoveValid (pieceX, pieceY) (toX, toY) =
  if | ( pieceY == (toY - 1) ) && (pieceX == toX) -> True
     | ( pieceY == (toY + 1) ) && (pieceX == toX) -> True
     | ( pieceX == (toX - 1) ) && (pieceY == toY) -> True
     | ( pieceX == (toX + 1) ) && (pieceY == toY) -> True
     | otherwise -> False

-- TODO: Proper game logic:
attack : Piece -> Piece -> Model -> Model
attack attacker defender model =
  if | attacker.value > defender.value -> attackerWins attacker defender model
     | defender.value > attacker.value -> updatePieces {attacker | inPlay <- False} model
     | otherwise -> updatePieces {attacker | inPlay <- False} model
                    |> updatePieces {defender | inPlay <- False}

attackerWins : Piece -> Piece -> Model -> Model
attackerWins attacker defender model =
  updatePieces {attacker | coord <- defender.coord} model
  |> updatePieces {defender | inPlay <- False}

-- first filter the list for pieces that are inPlay
-- then look for the one at given x,y location
-- then take the head, since there should only be one
getPieceByLocation : List Piece -> (Int, Int) -> Maybe Piece
getPieceByLocation pieces (x,y) =
  List.filter (\n -> n.inPlay) pieces
  |> List.filter (\n -> if n.coord == (x,y) then True else False)
  |> List.head

-- mark the given coordinate as the selected piece
markSelected : (Int, Int) -> Model -> Model
markSelected (x,y) model =
   { model | pieceIsSelected <- True
           , selectedPieceCoord <- (x,y) }
  
gameMessage: String -> Element
gameMessage message =
  Text.fromString message
  |> centered
