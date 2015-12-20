import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Array exposing (..)
import Mouse
import Text

type alias Piece =
  { id : Int
    , coord : (Int, Int)
    , value : Int
    , color : Color
    , inPlay : Bool
  }

type alias Model =
  { pieceIsSelected : Bool
    , selectedPieceCoord : (Int, Int)
    , pieces : List Piece
    , message: String
  }
  
-- TODO: automatically increment the pieceId
makePiece : Int -> (Int, Int) -> Int -> Color -> Piece
makePiece pid location val col =
  { id = pid
    ,coord = location
    , value = val
    , color = col
    , inPlay = True
  }
  
blues = [ (makePiece 1 (0,0) 4 blue), (makePiece 2 (1,0) 5 blue), (makePiece 3 (2,0) 6 blue), (makePiece 4 (3,0) 3 blue) ]

reds = [ (makePiece 5 (0,4) 4 red), (makePiece 6 (1,4) 5 red), (makePiece 7 (2,4) 6 red), (makePiece 8 (3,4) 3 red) ]

initBoard : Model
initBoard = 
  { pieceIsSelected = False
    , selectedPieceCoord = (0,0)
    , pieces = blues ++ reds
    , message = "Start Game!"
  }
          
columns = 4
rows = 5
boardWidth = 200
boardHeight = 250
columnMin = (boardWidth//2) * -1
columnMax = boardWidth // 2
columnWidth = boardWidth // columns
columnSpacing = columnWidth // 2

rowMin = (boardHeight//2)
rowMax = (boardHeight//2) * -1
rowHeight = boardHeight // rows
rowSpacing = rowHeight // 2

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
            
{--
            case defender of
              Nothing ->
              updatePieces {n | coord <- selected} model
              |> resetPieceSelected
              
              Just m ->
                attack n m model 
                |> resetPieceSelected
--}
                
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
  collage boardWidth boardHeight
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

    
-- Figure out which square was clicked, basically translate mouse coords to grid coords
spaceClicked : (Int, Int) -> (Int, Int)
spaceClicked  (x,y) =
  (x//columnWidth, y//rowHeight)
  
gridCoordsToModelIndex : (Int, Int) -> Int
gridCoordsToModelIndex (column, row) = 
  row * columns + column
  

-- make a path for the given column grid-line number  
getCol : Int -> Path
getCol colNum =
  path [
         (,) (toFloat (columnMin + columnWidth * colNum)) (toFloat rowMin)
        ,(,) (toFloat (columnMin + columnWidth * colNum)) (toFloat rowMax)
       ]
  
-- TODO: refactor the two draw functions to use a conditional to combine them into one function
-- For each column number, create a Form from a Path
drawCols : List Int -> List Form
drawCols colNums =
  List.map (\n -> traced (dashed blue) (getCol n)) colNums 
  
-- make a path for the given row grid-line number  
getRow : Int -> Path
getRow rowNum =
  path [
         (,) (toFloat columnMin) (toFloat (rowMin - rowHeight * rowNum))
        ,(,) (toFloat columnMax) (toFloat (rowMin - rowHeight * rowNum))
       ]
       
-- For each row number, create a Form from a Path
drawRows : List Int -> List Form
drawRows rowNums =
  List.map (\n -> traced (dashed blue) (getRow n)) rowNums

placePiece: Piece -> Form
placePiece piece =
  show piece.value
  |> color piece.color
  |> toForm
  |> move (calcMove piece.coord)
  
calcMove: (Int, Int) -> (Float, Float)
calcMove (x,y) = ( (getMoveX x), (getMoveY y) )

-- helper function to simplify the code gettig the x coordinate for a piece
getMoveX: Int -> Float
getMoveX x = calcX x
             |> toFloat

-- helper function to simplify the code gettig the y coordinate for a piece
getMoveY: Int -> Float
getMoveY y = calcY y
             |> toFloat

-- calculate the x coordinate for piece based on its column number and board goemetry
calcX: Int -> Int
calcX columnNo  = columnMin + (columnNo * columnWidth + columnSpacing)

-- calculate the y coordinate for piece based on its row number and board geometry
calcY rowNo = rowMin - (rowNo * rowHeight + rowSpacing)

  
-- First filter the list for pieces that are inPlay then show them
placePieces: List Piece -> List Form
placePieces l =
  List.map placePiece (List.filter (\n -> n.inPlay) l)
  
gameMessage: String -> Element
gameMessage message =
  Text.fromString message
  |> centered
  
-- List.map (\n -> move (-50,50) (toForm (show n)) ) l
