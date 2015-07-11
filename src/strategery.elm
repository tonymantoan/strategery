import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Array exposing (..)

model = [ 4,5,6,3,
          0,0,0,0,
          0,0,0,0,
          0,0,0,0,
          6,5,4,3 ]
          
columns = 4
rows = 5
boardWidth = 200
boardHeight = 250
columnMin = (boardWidth//2) * -1
columnMax = boardWidth // 2
columnWidth = boardWidth // columns
columnSpacing = columnWidth // 2

rowMin = (boardHeight//2) * -1
rowMax = (boardHeight//2)
rowHeight = boardHeight // rows
rowSpacing = rowHeight // 2

main =
  collage boardWidth boardHeight
    ( 
      (drawCols [0..columns]) ++ 
      (drawRows [0..rows]) ++
      (placePieces model)
    )

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
         (,) (toFloat columnMin) (toFloat (rowMin + rowHeight * rowNum))
        ,(,) (toFloat columnMax) (toFloat (rowMin + rowHeight * rowNum))
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
calcY rowNo = rowMin + (rowNo * rowHeight + rowSpacing)

  
placePieces: List Int -> List Form
placePieces l =
  Array.toList (Array.indexedMap placePiece (Array.fromList l))
  -- List.map (\n -> move (-50,50) (toForm (show n)) ) l
