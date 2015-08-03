import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Array exposing (..)
import Mouse

type alias Model =
  { pieceIsSelected : Bool
    , selectedPieceIndex : Int
    , pieces : List Int
  }

initBoard : Model
initBoard = 
  { pieceIsSelected = False
    , selectedPieceIndex = 0
    , pieces =
        [ 4,5,6,3,
          0,0,0,0,
          0,0,0,0,
          0,0,0,0,
          6,5,4,3 ]
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

-- TODO: reset piece selected after a piece has been moved  
update : (Int,Int) -> Model -> Model
update mousePosition  model =
  if model.pieceIsSelected == False then
    -- if no piece is selected: get the clicked piece and make that the selection, return the model as is
    markSelected  (gridCoordsToModelIndex (spaceClicked mousePosition)) model
  else
    -- if a piece is already selected: get the clicked pice this time, mark the other space as 0, make this space = the higher of the two
    let selected = gridCoordsToModelIndex (spaceClicked mousePosition)
        attacker = getValueAt  model.selectedPieceIndex model.pieces
        defender = getValueAt  (gridCoordsToModelIndex (spaceClicked mousePosition)) model.pieces
    in
        case attacker of
          Nothing -> model
          
          Just n ->
            case defender of
              Nothing -> model
              
              Just m ->
                updatePieces model.selectedPieceIndex 0 model
                |> attack n m selected 
                |> resetPieceSelected
    
    --{ model | pieces <- (List.reverse model.pieces) }

view : Model -> Element    
view model =
  collage boardWidth boardHeight
    ( 
      (drawCols [0..columns]) ++ 
      (drawRows [0..rows]) ++
      (placePieces model.pieces)
    )
    
updatePieces : Int -> Int -> Model -> Model
updatePieces index value model =
  {model | pieces <- ((List.take index model.pieces) ++ [value] ++ (List.drop (index+1) model.pieces))}
  
resetPieceSelected : Model -> Model
resetPieceSelected model =
  { model | pieceIsSelected <- False }
   
attack : Int -> Int -> Int -> Model -> Model
attack attacker defender index model =
  if | attacker > defender -> updatePieces index attacker model
     | defender > attacker -> updatePieces index defender model
     | otherwise -> updatePieces index 0 model
  
-- TODO: not thrilled about this function, maybe the pieces should be in an array instead of list
-- Just go back to using the array functions and get rid of this
getValueAt : Int -> List Int -> Maybe Int
getValueAt index list =
  List.head (List.drop index list)
    
-- mark the given index as the selected piece
markSelected : Int -> Model -> Model
markSelected index model =
   { model | pieceIsSelected <- True
           , selectedPieceIndex <- index }

    
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

placePiece: Int -> a -> Form
placePiece index pieceValue =
  move (getMoveX(index), getMoveY(index) ) (toForm (show pieceValue))
  
-- helper function to simplify the code gettig the x coordinate for a piece
getMoveX: Int -> Float
getMoveX index = calcColumnNum(index)
                 |> calcX
                 |> toFloat

-- helper function to simplify the code gettig the y coordinate for a piece
getMoveY: Int -> Float
getMoveY index = calcRowNum(index)
                 |> calcY
                 |> toFloat

-- calculate which column number the given index corrosponds to
calcColumnNum: Int -> Int
calcColumnNum index = if index < columns then index
                      else (index % columns)

-- calculate which row the given index corrosponds to
calcRowNum: Int -> Int
calcRowNum index = if index < columns then 0
                   else (index // columns)
                   
-- calculate the x coordinate for piece based on its column number and board goemetry
calcX: Int -> Int
calcX columnNo  = columnMin + (columnNo * columnWidth + columnSpacing)

-- calculate the y coordinate for piece based on its row number and board geometry
calcY rowNo = rowMin - (rowNo * rowHeight + rowSpacing)

  
placePieces: List Int -> List Form
placePieces l =
  Array.toList (Array.indexedMap placePiece (Array.fromList l))
  -- List.map (\n -> move (-50,50) (toForm (show n)) ) l
