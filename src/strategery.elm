import Color exposing (..)
import Graphics.Collage
import Graphics.Element exposing (..)
import Graphics.Input exposing (button)
import Array exposing (..)
import Mouse
import Text

import GamePieces exposing (..)
import Board exposing (..)
import Model exposing (..)
import Rules exposing (..)
import Utils

-- Board for playing the game.
board : Board
board = makeBoard 10 10 500 500 0 0

-- This is the tray for pieces that are captured (out of play).
tray : Board
tray = makeBoard 10 10 400 400 500 0

-- Create mailbox for the button that advances the game state.
stageButtonBox : Signal.Mailbox Int
stageButtonBox = Signal.mailbox 0

-- Create a type that represents either kind of user input.
type UI = MouseClick (Int,Int) | StageButton Int

-- Create one signal for all user interactions to feed into foldp.
userActions : Signal UI
userActions =
    Signal.merge
        (Signal.map MouseClick (Signal.sampleOn Mouse.clicks Mouse.position) )
        (Signal.map StageButton stageButtonBox.signal)

-- foldp func(anyType, stateObj, stateObj) state signal
-- create a signal of Models with foldp, which is maped to the view function
main =
  Signal.map view (Signal.foldp update initGame userActions )
  
-- UPDATE

update : UI -> Model -> Model
update action model =
  case action of
    MouseClick mousePosition ->
      if model.stage == over || model.turnComplete == True then
        model
      else if model.pieceIsSelected == False then
        handleClickWithoutSelected mousePosition model
      else
        handleClickWithSelected mousePosition model
        
    StageButton val ->
      if model.stage == over then
        model
      else
        handleStageButtonPress val model
    
selectPiece : Model -> (Int, Int) -> Maybe Piece
selectPiece model (mouseX, mouseY) =
  if (model.stage == setup) && mouseX > board.width then
   Utils.getPieceByLocation (List.filter (\n -> n.color == model.turn) model.pieces) (spaceClicked (mouseX, mouseY) tray)
  else
    Utils.getPieceByLocation (List.filter (\n -> n.color == model.turn) model.pieces) (spaceClicked (mouseX, mouseY) board)
    
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
      attacker = Utils.getPieceByLocation model.pieces model.selectedPieceCoord 
      defender = Utils.getPieceByLocation model.pieces selected
  in
      case attacker of
        Nothing -> model

        Just n ->
          if ( isMoveValid n selected board.rows model ) == True then
            handleMove n defender selected model
          else
            resetPieceSelected {model | message <- "Invalid move"}
            
handleStageButtonPress : Int -> Model -> Model
handleStageButtonPress val model =
  if model.stage == setup then
    if model.turn == blue then
      {model | turn <- red, message <- "Place Red Pieces"}
    else
      {model | stage <- started, turn <- gray, message <- "Red's Turn", buttonMessage <- "Ready"}
    
  else
    if model.turn == gray then
      {model | turn <- model.whoIsNext, turnComplete <- False, buttonMessage <- "Done"}
      |> resetReveal

    else if model.turn == blue then
      {model | turn <- gray, whoIsNext <- red, message <- "Red's turn", buttonMessage <- "Ready"}

    else 
      {model | turn <- gray, whoIsNext <- blue, message <- "Blue's turn", buttonMessage <- "Ready"}
    
resetReveal : Model -> Model
resetReveal model =
  {model | pieces <- ( List.map (\piece -> if piece.color == model.turn then 
                        {piece | reveal <- False}
                      else piece
                     ) model.pieces )
  }
  
-- mark the given coordinate as the selected piece
markSelected : (Int, Int) -> Model -> Model
markSelected (x,y) model =
   { model | pieceIsSelected <- True
           , selectedPieceCoord <- (x,y) }

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
      (placePieces (List.filter (\n -> False == n.inPlay) model.pieces) placePiece model.turn tray)
    )

{--
  flow right (List.map (\n-> outOfPlayDisplay n ) (List.filter (\n-> False == n.inPlay) model.pieces) )
  
outOfPlayDisplay : Piece -> Element
outOfPlayDisplay piece =
  displayPiece piece.value
  |> size (board.columnWidth // 2) (board.rowHeight // 2)
  |> color piece.color
--}
    
gameBoard : Model -> Element
gameBoard model = flow down [
  Graphics.Collage.collage board.width board.height
      ( 
        (drawCols board) ++ 
        (drawRows board) ++
        (placePieces (List.filter (\n -> n.inPlay) model.pieces) placePiece model.turn board) ++
        (makeNoGoSpaces noGo board)
      )
      , gameMessage model.message
      , button (Signal.message stageButtonBox.address 1) model.buttonMessage
    ]
  
gameMessage: String -> Element
gameMessage message =
  Text.fromString message
  |> centered
