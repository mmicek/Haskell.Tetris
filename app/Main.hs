module Main where

import Blocks
import BoardRenderer
import GameBoard
import GameLogic
import Graphics.Gloss
import KeyListener
import State

window :: Display
window = InWindow "Hetris" (1024, 768) (0, 0)

background :: Color
background = black

main :: IO ()
main = play window black 60 initialGameState render handleKeys updateGameState

renderTMPFoo state = renderBoard emptyBoard
