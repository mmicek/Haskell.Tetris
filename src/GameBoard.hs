{-|
Module : GameBoard
|-}
module GameBoard
  ( cellColor
  , numberRows
  , numberCells
  , Board(..)
  , Row(..)
  , Cell(..)
  , emptyRow
  , emptyBoard
  , rowsToBoard
  ) where

import           Blocks         ()
import           Graphics.Gloss

data Cell
  = Empty
  | FilledWith Color
  deriving (Show, Eq)
-- | Grid row definition
data Row =
  RowOfCells [Cell]
  deriving (Show)

-- | Grid definition
data Board =
  BoardOfRows [Row]
  deriving (Show)

-- | Numbering the rows in the board
numberRows :: Board -> [(Float, Row)]
numberRows (BoardOfRows rows) = zip [0 .. 21] rows

-- | Numbering cells in the row
numberCells :: Row -> [(Float, Cell)]
numberCells (RowOfCells cells) = zip [0 .. 9] cells

-- | reassembling board with rows
rowsToBoard :: [(Float, Row)] -> Board
rowsToBoard x = BoardOfRows (unZip x)

-- | unZip function for rows
unZip :: [(Float, Row)] -> [Row]
unZip = foldr (\x -> (++) [snd x]) []

-- | function returning color of the cell
cellColor :: Cell -> Color
cellColor Empty              = black
cellColor (FilledWith color) = color

-- | empty row definition
emptyRow :: Row
emptyRow = RowOfCells (replicate 10 Empty)

-- | empty board definition
emptyBoard :: Board
emptyBoard = BoardOfRows (replicate 22 emptyRow)
