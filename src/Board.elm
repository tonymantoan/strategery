module Board where

import Color
import Graphics.Element exposing (color, show)
import Graphics.Collage exposing (..)
import GamePieces exposing (Piece)

columns = 10
rows = 10
boardWidth = 500
boardHeight = 500
columnMin = (boardWidth//2) * -1
columnMax = boardWidth // 2
columnWidth = boardWidth // columns
columnSpacing = columnWidth // 2

rowMin = (boardHeight//2)
rowMax = (boardHeight//2) * -1
rowHeight = boardHeight // rows
rowSpacing = rowHeight // 2

noGo = [ (2,4), (2,5), (3,4), (3,5), (6,4), (6,5), (7,4), (7,5) ]

-- Figure out which square was clicked, basically translate mouse coords to grid coords
spaceClicked : (Int, Int) -> (Int, Int)
spaceClicked  (x,y) =
  (x//columnWidth, y//rowHeight)

-- TODO: This can probably be removed  
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
  List.map (\n -> traced (dashed Color.blue) (getCol n)) colNums 
  
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
  List.map (\n -> traced (dashed Color.blue) (getRow n)) rowNums
  
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
  
makeNoGoSpaces: List (Int, Int) -> List Form
makeNoGoSpaces noGoList =
  List.map (\n -> rect ( toFloat (columnWidth - 1)) (toFloat (rowHeight - 1) )
                  |> filled Color.black
                  |> move (calcMove n)
           ) noGoList