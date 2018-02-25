module ConwayBoardSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.Runner
import Control.Monad.State.Lazy
import Data.Maybe (isJust, fromJust)

import qualified Data.Foldable as F (length)

import ConwayBoard
import ConwayCell

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = do
    boardCreationTests
    emptyBoardTests
    populatedBoardTests
    neighborsTest
    liveNeighboursTest

boardCreationTests :: Spec
boardCreationTests = describe "ConwayBoard constructor" $ do
    it "should only allow nonzero and non-negatives dimensions" $ do
        let invalidDimensions = [(0, 3), (3, 0), (0, 0), (-1, 5), (5, -1), (-5, -5)]
        any isJust (newBoard <$> invalidDimensions) `shouldBe` False
    it "should return the correct size" $ do
        let boards = newBoard <$> [(3, 3), (1, 5), (5, 1)]
        map (liftM boardSize) boards `shouldBe` [Just (3, 3), Just (1, 5), Just (5, 1)]

emptyBoardTests :: Spec
emptyBoardTests = describe "Empty ConwayBoard" $ do
    let defaultBoard = unsafeNewBoard (5,5)
    it "should be initialized with only dead cells" $ 
        any isLiveCell defaultBoard `shouldBe` False
    it "should have the correct length" $
        F.length defaultBoard `shouldBe` 25
    it "should return Nothing for negative indexes" $ do
        atPosition (-1,  0 ) defaultBoard `shouldBe` Nothing
        atPosition ( 0, -1 ) defaultBoard `shouldBe` Nothing
        atPosition ( 0,  5 ) defaultBoard `shouldBe` Nothing
        atPosition ( 5,  0 ) defaultBoard `shouldBe` Nothing

populatedBoardTests :: Spec
populatedBoardTests = describe "Populated ConwayBoard" $ do
    let board = (`execState` unsafeNewBoard (2,2)) $ do
                                                            modify (setLiveCell (0, 0))
                                                            modify (setLiveCell (1, 1))
    let atPositionB = (`atPosition` board)
    it "should return the correct cell at position" $ do
        atPositionB (0, 0) `shouldBe` Just liveCell
        atPositionB (0, 1) `shouldBe` Just deadCell
        atPositionB (1, 0) `shouldBe` Just deadCell
        atPositionB (1, 1) `shouldBe` Just liveCell

neighborsTest :: Spec
neighborsTest = describe "Testing the neighbours" $ do
    let b = unsafeNewBoard (3, 3)
    it "should return the correct count of neighbours for each position" $ do
        length (cellNeighbours (0, 0) b) `shouldBe` 3
        length (cellNeighbours (1, 0) b) `shouldBe` 5
        length (cellNeighbours (1, 1) b) `shouldBe` 8

liveNeighboursTest :: Spec
liveNeighboursTest = describe "Testing the live neighbours" $ do
    -- _ _ x
    -- _ x x
    -- _ x _
    let b = (`execState` unsafeNewBoard (3, 3)) $ do
                                                        modify (setLiveCell (2, 0))
                                                        modify (setLiveCell (1, 1))
                                                        modify (setLiveCell (2, 1))
                                                        modify (setLiveCell (1, 2))
    it "should return the correct count of live neighbours" $ do
        countLiveNeighbours (0, 0) b `shouldBe` 1
        countLiveNeighbours (0, 2) b `shouldBe` 2
        countLiveNeighbours (2, 1) b `shouldBe` 3



unsafeNewBoard :: BoardSize -> Board
unsafeNewBoard = fromJust . newBoard