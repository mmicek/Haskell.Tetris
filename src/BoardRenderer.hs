{-|
Module : BoardRenderer
|-}
module BoardRenderer
  ( renderWall
  , render
  , renderBlock
  , renderBoard
  , cellsCoords
  , toScreenCoords
  , cellSize
  ) where

import           Blocks
import           GameBoard
import           Graphics.Gloss
import           State

-- | cell size
cellSize :: Float
cellSize = 32

-- | wall color
wallColor :: Color
wallColor = greyN 0.3

-- | board width
boardWidth :: Float
boardWidth = 10 * cellSize

-- | board height
boardHeight :: Float
boardHeight = 21 * cellSize

-- | board padding
padding :: Float
padding = 70

-- | board color
boardColor :: Color
boardColor = black

-- | converter from board cords to global 'screen' cords
toScreenCoords :: (Float, Float) -> (Float, Float)
toScreenCoords (x1, y1) = (x2, y2)
  where
    x2 = x1 * cellSize - 5 * cellSize
    y2 = 11 * cellSize - (y1 * cellSize)

-- | function rendering wall
renderWall :: Picture
renderWall =
  pictures
    [ translate (-cellSize / 2) 0 $color wallColor $
      rectangleSolid (boardWidth + padding) (boardHeight + padding)
    , translate (-cellSize / 2) 0 $
      color boardColor $ rectangleSolid boardWidth boardHeight
    ]

-- | returning list of the cells with its coordinates
cellsCoords :: Board -> [(Float, Float, Cell)]
cellsCoords board = concatMap extractRows (numberRows board)
  where
    extractRows :: (Float, Row) -> [(Float, Float, Cell)]
    extractRows (y, row) = map extractCells (numberCells row)
      where
        extractCells (x, c) = (x, y, c)

-- | rendering cell literally creating picture for gloss
renderCell :: (Float, Float) -> Color -> Picture
renderCell (x, y) col = translate x' y' $ color col $ rectangleSolid size' size'
  where
    x' = fst $ toScreenCoords (x, y)
    y' = snd $ toScreenCoords (x, y)
    size' = cellSize * 0.8

-- | turning board to the picture
renderBoard :: Board -> Picture
renderBoard board = pictures $ map cellToPic $ cellsCoords board
  where
    cellToPic (x, y, cell)
      | y < 1 = pictures []
      | cell == Empty = pictures []
      | otherwise = renderCell (x, y) (cellColor cell)

-- | moving block as temp cell on the board
renderBlock :: Block -> (Float, Float) -> Board -> Board
renderBlock block (x, y) board = BoardOfRows $ map renderRow $ numberRows board
  where
    renderRow (yP, row) = RowOfCells $ map renderCellsInRows (numberCells row)
      where
        renderCellsInRows (xP, cell)
          | cell /= Empty = cell
          | blockHasCoord (xP - x, yP - y) block = FilledWith (blockColor block)
          | otherwise = Empty

-- | turning whole state to the picture
render :: State -> Picture
render state
  | gameOver state =  pictures [walls, currentBoard, gameOverMessage, gameOverScore]
  | otherwise = pictures [walls, currentBoard, activeBlock,playerScore]
    where
      walls = renderWall
      currentBoard = renderBoard $ gameBoard state
      activeBlock =
        renderBoard $ renderBlock (block state) (blockPos state) (gameBoard state)
      gameOverMessage =translate (-200.0) 50.0 $ scale 0.5 0.5 (pictures [color (light rose) (Text "You Lost")])
      gameOverScore = translate (-250.0) (-50.0) $ scale 0.5 0.5 (pictures [color (light rose) (Text ("Your socre: " ++ show(score state)))])
      playerScore = translate 200.0 200.0 (scale 0.2 0.2 (pictures [playerScoreText]))
        where
          playerScoreText = color white (Text scoreText)
          scoreText = "SCORE: " ++ show (score state)
