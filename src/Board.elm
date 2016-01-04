module Board where

import Color
import Text
import Graphics.Element exposing (Element, size, centered, color, show)
import Graphics.Collage exposing (..)
import GamePieces exposing (Piece, flag, spy, bomb)
import Utils exposing (..)

type alias Board =
  { columns : Int
    , rows : Int
    , width : Int
    , height: Int
    , columnMin: Int
    , columnMax: Int
    , columnWidth: Int
    , columnSpacing: Int
    , rowMin: Int
    , rowMax: Int
    , rowHeight: Int
    , rowSpacing: Int
    , offsetX: Int
    , offsetY: Int
  }
  
makeBoard : Int -> Int -> Int -> Int -> Int -> Int -> Board
makeBoard c r w h ox oy =
  { columns = c
    , rows = r
    , width = w
    , height = h
    , columnMin = (w//2) * -1
    , columnMax = w // 2
    , columnWidth = w // c
    , columnSpacing = (w // c) // 2
    , rowMin = (h//2)
    , rowMax = (h//2) * -1
    , rowHeight = h // r
    , rowSpacing = (h // r) // 2
    , offsetX = ox
    , offsetY = oy
  }
  

noGo = [ (2,4), (2,5), (3,4), (3,5), (6,4), (6,5), (7,4), (7,5) ]

{-| Figure out which square was clicked, basically translate mouse coords to grid coords.
-}
spaceClicked : (Int, Int) -> Board -> (Int, Int)
spaceClicked  (x,y) board =
  ( (x - board.offsetX) // board.columnWidth, (y - board.offsetY) // board.rowHeight)

{-| Make a path for the given column grid-line number.
-}
getCol : Int -> Board -> Path
getCol colNum board =
  path [
         (,) (toFloat (board.columnMin + board.columnWidth * colNum)) (toFloat board.rowMin)
        ,(,) (toFloat (board.columnMin + board.columnWidth * colNum)) (toFloat board.rowMax)
       ]
  
-- TODO: refactor the two draw functions to use a conditional to combine them into one function
{-| For each column number, create a Form from a Path.
-}
drawCols : Board -> List Form
drawCols board =
  List.map (\n -> traced (dashed Color.blue) (getCol n board)) [0..board.columns] 
  
{-| Make a path for the given row grid-line number.
-}
getRow : Int -> Board -> Path
getRow rowNum board =
  path [
         (,) (toFloat board.columnMin) (toFloat (board.rowMin - board.rowHeight * rowNum))
        ,(,) (toFloat board.columnMax) (toFloat (board.rowMin - board.rowHeight * rowNum))
       ]
       
{-| For each row number, create a Form from a Path.
-}
drawRows : Board -> List Form
drawRows board =
  List.map (\n -> traced (dashed Color.blue) (getRow n board)) [0..board.rows]

{-| Position the visual representation of piece on the board.
-}
placePiece: Piece -> Color.Color -> Board -> Form
placePiece piece turn board =
  (if turn == piece.color || piece.reveal == True || piece.inPlay == False then
    displayPiece piece.value
  else 
    centered (Text.fromString "") ) 
  |> size (board.columnWidth // 2) (board.rowHeight // 2)
  |> color piece.color
  |> toForm
  |> move (calcMove piece.coord board)

{-| Create a displayable representation of the piece based on its value.
-}
displayPiece: Int -> Element
displayPiece value =
  if value == flag then
    centered (Text.fromString "F")
  else if value == spy then
    centered (Text.fromString "S")
  else if value == bomb then
    centered (Text.fromString "B")
  else
    centered (Text.fromString (toString value))

{-| Calculate the x,y coords of a piece based on board geometry.
-}
calcMove: (Int, Int) -> Board -> (Float, Float)
calcMove (x,y) board = ( (getMoveX x board), (getMoveY y board) )

{-| helper function to simplify the code gettig the x coordinate for a piece
-}
getMoveX: Int -> Board -> Float
getMoveX x board = ( board.columnMin + (x * board.columnWidth + board.columnSpacing) )
             |> toFloat

{-| helper function to simplify the code gettig the y coordinate for a piece.
-}
getMoveY: Int -> Board -> Float
getMoveY y board = ( board.rowMin - (y * board.rowHeight + board.rowSpacing) )
             |> toFloat
  
{-| First filter the list for pieces that are inPlay then show them.
-}
placePieces: List Piece -> (Piece -> Color.Color -> Board -> Form) -> Color.Color -> Board -> List Form
placePieces l makeForm turn board =
  List.map (\n -> makeForm n turn board ) l

{-| Place a visual representation of unusable spaces on the board.
-}
makeNoGoSpaces: List (Int, Int) -> Board -> List Form
makeNoGoSpaces noGoList board =
  List.map (\n -> rect ( toFloat (board.columnWidth - 1)) (toFloat (board.rowHeight - 1) )
                  |> filled Color.black
                  |> move (calcMove n board)
           ) noGoList

{-| Check if a given coordinate is a 'no-go' space.
-}
isNotInNoGoZone : (Int, Int) -> Bool
isNotInNoGoZone coord =
  List.isEmpty ( List.filter (\n -> if n == coord then True else False) noGo )

{-| Check if there are any other pieces or no-go spaces between two coordinates.
-}
hasClearPath : (Int, Int) -> (Int, Int) -> List Piece -> Bool
hasClearPath (fromX, fromY) (toX, toY) pieces =
  if isWithinOne (fromX, fromY) (toX, toY) then
    True
  else
    if fromX == toX then
      isBlocked ( checkYaxisBlock fromX fromY toY ((getInPlayCoords pieces) ++ noGo) )
    else
      isBlocked ( checkXaxisBlock fromY fromX toX ((getInPlayCoords pieces) ++ noGo) )