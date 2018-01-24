{-|
Module : GameLogic
|-}
module GameLogic
  ( updateGameState
  , blockCoordList
  , mapColision
  , isNotColision
  , isRowFull
  , convert
  , bottomWallColision
  ) where

import           Blocks
import           BoardRenderer
import           GameBoard
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort ()
import           State
import           System.Random

-- | horizontal block speed constant
blockVel :: Float
blockVel = 5

-- | Unity like move time
blockMovTime :: State -> Float
blockMovTime state
  | gameOver state = 0
  | otherwise = 1.0 / blockVel

-- | game state updater
updateGameState :: Float -> State -> State
updateGameState tm state = updater state {time = time state + tm, dTime = tm}

-- | simple updating given state
updater :: State -> State
updater state
  | gameOver state = gameOverState {score = score state, gameBoard = gameBoard state}
  | timeToNextMove (updateClock state) <= 0 =
    moveBlock (updateClock state) {timeToNextMove = blockMovTime state}
  | otherwise = state {timeToNextMove = timeToNextMove state - dTime state}
  where
    updateClock s =
      state {timeToNextMove = timeToNextMove state - dTime s}

-- | removing full rows
removeFullRows :: State -> State
removeFullRows state = checkBoard (numberRows (gameBoard state)) state

-- | fixing board eq. removing empty rows
checkBoard :: [(Float, Row)] -> State -> State
checkBoard [] state = state
checkBoard (x:xs) state =
  if isRowFull (numberCells (snd x))
    then removeFullRows
           (state
            { score = score'
            , gameBoard =
                rowsToBoard
                  (removeRow (numberRows (gameBoard state)) emptyRow (fst x))
            })
    else checkBoard xs state
  where
    score' = score state + 100

-- | function removing the rows
removeRow :: [(Float, Row)] -> Row -> Float -> [(Float, Row)]
removeRow [] _ _ = []
removeRow (x:xs) lastRow fullrow
  | fst x > fullrow = x : xs
  | fst x == 0 = (0, emptyRow) : removeRow xs (snd x) fullrow
  | otherwise = (fst x, lastRow) : removeRow xs (snd x) fullrow

-- | checker if row is full
isRowFull :: [(Float, Cell)] -> Bool
isRowFull = foldr (\x -> (&&) (cellColor (snd x) /= black)) True

-- | horizontal block mover
moveBlock :: State -> State
moveBlock state =
  if isNotColision state {blockPos = (x, y + 1)}
    then state {blockPos = (x, y')}
    else removeFullRows $ loadNewState state
  where
    (x, y) = blockPos state
    y' = y + 1

-- | checking if block is not in the collision
isNotColision :: State -> Bool
isNotColision state
  | snd (blockPos state) < 0 = True
  | bottomWallColision (blockCoordList state) ||
      mapColision state (blockCoordList state) = False
  | otherwise = True

-- | Did we reach the bottom ?
bottomWallColision :: [(Float, Float)] -> Bool
bottomWallColision [] = False
bottomWallColision (x:xs) = result || bottomWallColision xs
  where
    result = snd x > 21

-- | after collision we need to load new state of the game with new block etc.
loadNewState :: State -> State
loadNewState state
  | isNotColision (startState {blockPos = blockPos state }) = initialGameState --(fst (blockPos state),snd (blockPos state) + 7)
  | isGameOver state = gameOverState {score = score state, gameBoard = gameBoard state}
  | otherwise = startState
    where startState = state
                { blockPos = (4, 0)
                , gameBoard = renderBlock (block state) (blockPos state) (gameBoard state)
                , block = newBlock $ fst newSeed
                , randSeed = snd newSeed
                , gameOver = False
                }
                  where
                    newSeed = randomR (0.0, 1.0) (randSeed state)

-- | collisions on the board summary function
mapColision :: State -> [(Float, Float)] -> Bool
mapColision _ [] = False
mapColision state x =
  pointColision (head x) (numberRows (gameBoard state)) ||
  mapColision state (tail x)

-- | on point collision
pointColision :: (Float, Float) -> [(Float, Row)] -> Bool
pointColision _ [] = False
pointColision (pointx, pointy) x =
  if fst (head x) == pointy
    then colision pointx (numberCells (snd (head x)))
    else pointColision (pointx, pointy) (tail x)

-- | most basic collision with element on the board
colision :: Float -> [(Float, Cell)] -> Bool
colision _ [] = False
colision yx x =
  if yx == fst (head x)
    then cellColor (snd (head x)) /= black
    else colision yx (tail x)

-- | current sub-blocks cords
blockCoordList :: State -> [(Float, Float)]
blockCoordList state = listConvert (blockCords $ block state) state

-- | block new sub-blocks cords
listConvert :: [(Float, Float)] -> State -> [(Float, Float)]
listConvert [] _ = []
listConvert x state =
  convert (head x) (blockPos state) : listConvert (tail x) state

-- | to new position converter
convert :: (Float, Float) -> (Float, Float) -> (Float, Float)
convert (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

-- | Have you lost ?
isGameOver :: State -> Bool
isGameOver state = checkFirstRow (numberCells (snd (head (numberRows (gameBoard state)))))

-- | We must somehow check it
checkFirstRow :: [(Float,Cell)] -> Bool
checkFirstRow = foldr (\ x -> (||) (cellColor (snd x) /= black)) False
