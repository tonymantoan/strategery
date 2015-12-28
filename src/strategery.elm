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
    , stage: Int
    , turn: Color
  }
  
setup = 1
started = 2
over = 3

initGame : Model
initGame = 
  { pieceIsSelected = False
    , selectedPieceCoord = (0,0)
    , pieces = blues ++ reds
    , message = "Start Game!"
    , stage = setup
    , turn = blue
  }

board : Board
board = makeBoard 10 10 500 500 0 0

tray : Board
tray = makeBoard 10 10 400 400 500 0

-- foldp func(anyType, stateObj, stateObj) state signal
-- create a signal of Models with foldp, which is maped to the view function
main =
  Signal.map view (Signal.foldp update initGame (Signal.sampleOn Mouse.clicks Mouse.position) )
  
-- UPDATE

update : (Int,Int) -> Model -> Model
update mousePosition  model =
  if model.pieceIsSelected == False then
    handleClickWithoutSelected mousePosition model
  else
    handleClickWithSelected mousePosition model
    
selectPiece : Model -> (Int, Int) -> Maybe Piece
selectPiece model (mouseX, mouseY) =
  if (model.stage == setup) && mouseX > board.width then
    getPieceByLocation (List.filter (\n -> n.color == model.turn) model.pieces) (spaceClicked (mouseX, mouseY) tray)
  else
    getPieceByLocation (List.filter (\n -> n.color == model.turn) model.pieces) (spaceClicked (mouseX, mouseY) board)
    
handleClickWithoutSelected : (Int, Int) -> Model -> Model
handleClickWithoutSelected mousePosition model =
  -- if no piece is selected: get the clicked piece and make that the selection, return the model as is
  -- but first make sure the piece is movable
  let
    pieceClicked = selectPiece model mousePosition
  in
    case pieceClicked of
      Nothing -> model
        
      Just n ->
        if (model.stage == started) && (n.value == bomb || n.value == flag) then
         model
        else
          markSelected  n.coord model
          |> updatePieceSelectedMessage

handleClickWithSelected : (Int, Int) -> Model -> Model
handleClickWithSelected mousePosition model =
  -- if a piece is already selected: get the clicked piece this time, mark the other space as 0, make this space = the higher of the two
  let selected = (spaceClicked mousePosition board)
      attacker = getPieceByLocation model.pieces model.selectedPieceCoord 
      defender = getPieceByLocation model.pieces selected
  in
      case attacker of
        Nothing -> model

        Just n ->
          if ( isMoveValid n selected model ) == True then
            handleMove n defender selected model
          else
            resetPieceSelected {model | message <- "Invalid move"}
{--
  # VIEW
  The collage contains the game board itself.  Other elements show
  other game related info.
--}
view : Model -> Element    
view model = flow right [ (gameBoard model), (pieceTray model) ]

pieceTray : Model -> Element
pieceTray model =
  Graphics.Collage.collage tray.width tray.height
    (
      (drawCols tray) ++ 
      (drawRows tray) ++
      (placePieces (List.filter (\n -> False == n.inPlay) model.pieces) placePiece tray)
    )
    
gameBoard : Model -> Element
gameBoard model = flow down [
  Graphics.Collage.collage board.width board.height
      ( 
        (drawCols board) ++ 
        (drawRows board) ++
        (placePieces (List.filter (\n -> n.inPlay) model.pieces) placePiece board) ++
        (makeNoGoSpaces noGo board)
      ),
      gameMessage model.message
    ]
    
-- GAME LOGIC

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
    |> resetPieceSelected
              
    Just m ->
      attack attacker m model 
      |> resetPieceSelected

updatePieces :  Piece -> Model -> Model
updatePieces piece model =
  -- Make a new list with the old piece filtered out, then append the new piece
  {model | pieces <- [piece] ++ (List.filter (\n -> if n.id == piece.id then False else True) model.pieces)}

resetPieceSelected : Model -> Model
resetPieceSelected model =
  { model | pieceIsSelected <- False }
  
updatePieceSelectedMessage : Model -> Model
updatePieceSelectedMessage model = {model | message <- "Selected " ++ ( toString model.selectedPieceCoord ) }

isMoveValid : Piece -> (Int, Int) -> Model -> Bool
isMoveValid piece to model =
  if model.stage == setup then
    isMoveValidForSetup piece to model
  else
    isMoveValidForPlay piece to model

isMoveValidForSetup : Piece -> (Int, Int) -> Model -> Bool
isMoveValidForSetup piece (toX, toY) model =
  if piece.color == blue && toY < 4  && toX < board.rows then
    True
  else if piece.color == red && toY > 5 && toX < board.rows then
    True
  else
    False

isMoveValidForPlay : Piece -> (Int, Int) -> Model -> Bool
isMoveValidForPlay piece to model =
  if piece.value == scout then
    ( to |> isNotInNoGoZone ) && ( isLinearTo piece.coord to ) && ( hasClearPath piece.coord to model )
  else
    ( to |> isNotInNoGoZone ) && ( isWithinOne piece.coord to )
    
isNotInNoGoZone : (Int, Int) -> Bool
isNotInNoGoZone coord =
  List.isEmpty ( List.filter (\n -> if n == coord then True else False) noGo )
    
hasClearPath : (Int, Int) -> (Int, Int) -> Model -> Bool
hasClearPath (fromX, fromY) (toX, toY) model =
  if isWithinOne (fromX, fromY) (toX, toY) then
    True
  else
    if fromX == toX then
      isBlocked ( checkYaxisBlock fromX fromY toY ((getInPlayCoords model.pieces) ++ noGo) )
    else
      isBlocked ( checkXaxisBlock fromY fromX toX ((getInPlayCoords model.pieces) ++ noGo) )

getInPlayCoords : List Piece -> List (Int, Int)
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
