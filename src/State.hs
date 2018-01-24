{-|
Module : State
|-}
module State
  ( initialGameState
  , gameOverState
  , State(..)
  ) where

import           Blocks
import           GameBoard
import           System.Random

-- | Data describing current game state
data State = State
  { score          :: Integer -- ^ Current score
  , time           :: Float -- ^ Current time
  , dTime          :: Float -- ^ dTime passed
  , timeToNextMove :: Float -- ^ current time to the next move
  , gameBoard      :: Board -- ^ current board with cells
  , block          :: Block -- ^ current block we use
  , blockPos       :: (Float, Float) -- ^ current blocks position
  , randSeed       :: StdGen -- ^ random seed
  , gameOver       :: Bool -- ^ Have you already lost ?
  } deriving (Show)

-- | initial state of the game
initialGameState :: State
initialGameState =
  State
  { score = 0
  , time = 0
  , dTime = 0
  , timeToNextMove = 0
  , gameBoard = emptyBoard
  , blockPos = (5, 0)
  , block = newBlock 1
  , randSeed = mkStdGen 0
  , gameOver = False
  }

-- | if you lose you get this state
gameOverState :: State
gameOverState =
  State
  { score = 666
  , time = 0
  , dTime = 0
  , timeToNextMove = 666
  , gameBoard = emptyBoard
  , blockPos = (0,0)
  , block = newBlock 1
  , randSeed = mkStdGen 0
  , gameOver = True
  }
