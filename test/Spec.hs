import           BoardRenderer
import           GameBoard
import           GameLogic
import           Graphics.Gloss  ()
import           KeyListener
import           Test.HUnit
import           Test.QuickCheck

coordsCheck = quickCheck ((\(x,y) -> toScreenCoords (x,y) == (x*32 - 160, 352 - (y*32)) ) :: (Float,Float)->Bool)
numberCellsTest = quickCheck ((\x -> let n = round $ abs x in numberCells  (RowOfCells (replicate n  Empty)) == zip [0..9] (replicate n Empty)) :: (Float -> Bool))

test1 = TestCase (assertEqual "convertTest" (2,2) (convert (1,1) (1,1)) )
test2 = TestCase (assertEqual "cellSize" 32 cellSize )
test3 = TestCase (assertEqual "boundColision" False  (boundColision [(7,7),(10,10)] ))
test4 = TestCase (assertEqual "bottomColTest" True  (bottomWallColision [(5,20),(5,21),(5,22)] ) )

testList = TestList [TestLabel "test1" test1
                    ,TestLabel "test2" test2
                    ,TestLabel "test3" test3
                    ,TestLabel "test4" test4]

main :: IO()
main = do
  coordsCheck
  numberCellsTest

hUnitTest = runTestTT testList
