module Rules where

import Model exposing (..)
import GamePieces exposing (Piece, flag, spy, bomb, scout, marshal, miner)
import Utils exposing (..)
import Color
import Board exposing (isNotInNoGoZone, hasClearPath)

handleMove : Piece -> Maybe Piece -> (Int, Int) -> Model -> Model
handleMove attacker defender moveTo model =
  if model.stage == setup then
    handleMoveForSetup attacker defender moveTo model
  else
    handleMoveForPlay attacker defender moveTo model
    
handleMoveForSetup : Piece -> Maybe Piece -> (Int, Int) -> Model -> Model
handleMoveForSetup attacker defender moveTo model =
  case defender of
    Nothing ->
    updatePieces {attacker | coord <- moveTo} model
    |> resetPieceSelected
              
    Just m ->
      updatePieces {m | coord <- attacker.coord} model
      |> updatePieces {attacker | coord <- moveTo}
      |> resetPieceSelected

handleMoveForPlay : Piece -> Maybe Piece -> (Int, Int) -> Model -> Model
handleMoveForPlay attacker defender moveTo model =
  case defender of
    Nothing ->
    updatePieces {attacker | coord <- moveTo} model
    |> completeTurn
    |> resetPieceSelected
              
    Just m ->
      if attacker.color == m.color then
        {model | message <- "No friendly fire!"}
        |> resetPieceSelected
      else
        attack attacker m model
        |> completeTurn
        |> resetPieceSelected
        
completeTurn : Model -> Model
completeTurn model =
  {model | turnComplete <- True}

updatePieces :  Piece -> Model -> Model
updatePieces piece model =
  -- Make a new list with the old piece filtered out, then append the new piece
  {model | pieces <- [piece] ++ (List.filter (\n -> if n.id == piece.id then False else True) model.pieces)}

resetPieceSelected : Model -> Model
resetPieceSelected model =
  { model | pieceIsSelected <- False }
  
updatePieceSelectedMessage : Model -> Model
updatePieceSelectedMessage model = {model | message <- "Selected " ++ ( toString model.selectedPieceCoord ) }

isMoveValid : Piece -> (Int, Int) -> Int -> Model -> Bool
isMoveValid piece to rows model =
  if model.stage == setup then
    isMoveValidForSetup piece to rows model
  else
    isMoveValidForPlay piece to model

isMoveValidForSetup : Piece -> (Int, Int) -> Int -> Model -> Bool
isMoveValidForSetup piece (toX, toY) rows model =
  if piece.color == Color.blue && toY < 4  && toX < rows then
    True
  else if piece.color == Color.red && toY > 5 && toX < rows then
    True
  else
    False

isMoveValidForPlay : Piece -> (Int, Int) -> Model -> Bool
isMoveValidForPlay piece to model =
  if piece.value == scout then
    ( to |> isNotInNoGoZone ) && ( isLinearTo piece.coord to ) && ( hasClearPath piece.coord to model.pieces )
  else
    ( to |> isNotInNoGoZone ) && ( isWithinOne piece.coord to )
    
{--
  Battle rules.
  
  - if a miner is attacking bomb, the miner wins
  - if a spy is attacking the marshal (usually the 10) the spy wins
  - if it's not one of those special cases, high number wins
    - if the attacker wins, it takes the place of the defender
    - if the defender wins, the attacker is simply removed from play
  - if it's a tie, they are both removed
--}
attack : Piece -> Piece -> Model -> Model
attack attacker defender model =
  if | attacker.value == miner && defender.value == bomb -> attackerWins attacker defender model
     | attacker.value == spy && defender.value == marshal -> attackerWins attacker defender model
     | attacker.value > defender.value -> attackerWins attacker defender model
     | defender.value > attacker.value -> defenderWins attacker defender model
     | otherwise -> updatePieces {attacker | inPlay <- False, coord <- attacker.traySlot} model
                    |> updatePieces {defender | inPlay <- False, coord <- defender.traySlot}

attackerWins : Piece -> Piece -> Model -> Model
attackerWins attacker defender model =
  updatePieces {attacker | coord <- defender.coord, reveal <- True} model
  |> updatePieces {defender | inPlay <- False, coord <- defender.traySlot}
  |> checkForVictory defender

defenderWins : Piece -> Piece -> Model -> Model
defenderWins attacker defender model =
  updatePieces {attacker | inPlay <- False, coord <- attacker.traySlot} model
  |> updatePieces {defender | reveal <- True}
  |> checkForVictory attacker
  
checkForVictory : Piece -> Model -> Model
checkForVictory loser model =
  if loser.value == flag then
    {model | stage <- over, message <- (playerString model.turn) ++ " wins by capturing the flag"}
    |> revealAll
  else if False == ( anyMoveables (List.filter (\n-> n.color == loser.color) model.pieces) ) then
    {model | stage <- over, message <- (playerString model.turn) ++ " wins by eliminating all mobile pieces"}
    |> revealAll
  else
    model

anyMoveables : List Piece -> Bool
anyMoveables pieces =
  List.any (\n -> n.value > flag && n.value < bomb) pieces

revealAll : Model -> Model
revealAll model =
  {model | pieces <- (List.map (\n -> {n | reveal <- True}) model.pieces)}