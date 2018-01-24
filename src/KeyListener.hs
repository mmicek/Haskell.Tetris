{-|
Module : KeyListener
|-}
module KeyListener
  ( handleKeys
  , boundColision
  ) where

import           Blocks
import           GameLogic
import           Graphics.Gloss.Interface.Pure.Game
import           State

-- | simple key handler
handleKeys :: Event -> State -> State -- ^ Left key move right
handleKeys (EventKey (SpecialKey KeyLeft) pos _ _) state =
  if canUpdate (state {blockPos = (x - 1, y)}) && pos == Down
    then state {blockPos = (x', y)}
    else state
  where
    (x, y) = blockPos state
    x' = x - 1

-- | Right key move right
handleKeys (EventKey (SpecialKey KeyRight) pos _ _) state =
  if canUpdate (state {blockPos = (x + 1, y)}) && pos == Down
    then state {blockPos = (x', y)}
    else state
  where
    (x, y) = blockPos state
    x' = x + 1

-- | Down key force move down
handleKeys (EventKey (SpecialKey KeyDown) pos _ _) state =
  if pos == Down
    then moveDown state
    else state

-- | a key clockwise rotation
handleKeys (EventKey (Char 'a') pos _ _) state =
  if pos == Down && canUpdate (changeRotationCW state)
    then changeRotationCW state
    else state

-- | d key counter clockwise rotation
handleKeys (EventKey (Char 'd') pos _ _) state =
  if pos == Down && canUpdate (changeRotationCCW state)
    then changeRotationCCW state
    else state
handleKeys _ state = state

--  | function checking if we can do the update state operation
canUpdate :: State -> Bool
canUpdate state =
  boundColision (blockCoordList state) &&
  not (mapColision state (blockCoordList state))

-- | check if we colide with bound
boundColision :: [(Float, Float)] -> Bool
boundColision = foldr (\x -> (&&) (not (fst x > 9 || fst x < 0))) True

-- | super uber rabid down moving
moveDown :: State -> State
moveDown state = state {blockPos = (x, y + n)}
  where
    (x, y) = blockPos state
    n = countMovingDown state 0

-- some random foo
countMovingDown :: State -> Float -> Float
countMovingDown state n
  | isNotColision (state {blockPos = (x, y + n)}) = countMovingDown state (n + 1)
  | otherwise = n - 1
  where
    (x, y) = blockPos state

-- | clockwise rotation function
changeRotationCW :: State -> State
changeRotationCW state = state {block = rotateBlockCW $ block state}

-- | counter clockwise rotation
changeRotationCCW :: State -> State
changeRotationCCW state = state {block = rotateBlockCCW $ block state}
