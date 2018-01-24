{-|
Module : Blocks
|-}
module Blocks
  ( Block(..)
  , blockHasCoord
  , blockColor
  , rotateBlockCW
  , rotateBlockCCW
  , newBlock
  , blockCords
  ) where

import           Graphics.Gloss

-- | data representing abstract 2D block
data Block =
  BlockCoords [(Float, Float)] Color -- ^ blocks coordinates and blocks color
  deriving (Show)

-- | function returning blocks color
blockColor :: Block -> Color
blockColor (BlockCoords _ cl) = cl

-- | function returning blocks coordinates
blockCords :: Block -> [(Float, Float)]
blockCords (BlockCoords cor _) = cor

-- | checking if given pair (a,b) is in blocks coordinates
blockHasCoord :: (Float, Float) -> Block -> Bool
blockHasCoord coords (BlockCoords coords' _) = coords `elem` coords'

-- | getting new random block
newBlock :: Double -> Block
newBlock r =
  case truncate (r * 1000) `mod` 6 of
    0 -> BlockCoords [(0, 0), (0, 1), (-1, 0), (-1, 1)] (dim blue)
    1 -> BlockCoords [(0, 0), (1, 0), (0, 1), (0, -1)] (dark yellow)
    2 -> BlockCoords [(0, 0), (1, 0), (2, 0), (-1, 0)] (dim orange)
    3 -> BlockCoords [(0, 0), (1, 0), (0, -1), (-1, -1)] (dim green)
    4 -> BlockCoords [(0, 0), (0, 1), (1, 0), (-1, 0)] (dim red)
    5 -> BlockCoords [(0, 0), (0, -1), (-1, 0), (1, -1)] (dark (dim cyan))

-- | clockwise block rotation
rotateBlockCW :: Block -> Block
rotateBlockCW (BlockCoords cords col) = BlockCoords (map rotateCW cords) col
  where
    rotateCW (a, b) = (-b, a)

-- | counter clockwise block rotation
rotateBlockCCW :: Block -> Block
rotateBlockCCW (BlockCoords cords col) = BlockCoords (map rotateCW cords) col
  where
    rotateCW (a, b) = (b, -a)
