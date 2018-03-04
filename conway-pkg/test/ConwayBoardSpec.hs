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
    singleCellBoardTests
    boardCreationTests
    resetBoardTests
    emptyBoardTests
    populatedBoardTests
    neighborsTest
    liveNeighboursTest
    flattenSquareBoardTest

    singleCellShouldDieTest

    twoCellsDieTest
    minimalBlockStillLife
    heterogenousBlockStillLife
    blockStillLifeTest
    blinkerOscillatorTest

singleCellBoardTests :: Spec
singleCellBoardTests = describe "Single cell board tests" $ do
    let board = setLiveCell (0, 0) . unsafeNewBoard $ (1, 1)
    it "should kill a cell at specified position" $ do
        isLiveCell (0, 0) board `shouldBe` Just True
        isLiveCell (0, 0) (resetBoard board) `shouldBe` Just False


boardCreationTests :: Spec
boardCreationTests = describe "ConwayBoard constructor" $ do
    it "should only allow nonzero and non-negatives dimensions" $ do
        let invalidDimensions = [(0, 3), (3, 0), (0, 0), (-1, 5), (5, -1), (-5, -5)]
        any isJust (newBoard <$> invalidDimensions) `shouldBe` False
    it "should return the correct size" $ do
        let boards = newBoard <$> [(3, 3), (1, 5), (5, 1)]
        map (liftM boardSize) boards `shouldBe` [Just (3, 3), Just (1, 5), Just (5, 1)]

resetBoardTests :: Spec
resetBoardTests = describe "ConwayBoard Reset" $ do
    it "a reset board should be equal to a newly created one" $ do
        let board       = setLiveCells [ (0, 0), (1, 1) ] (unsafeNewBoard (2, 2))
        let rst  = resetBoard board
        rst `shouldBe` unsafeNewBoard (2,2)
    it "should kill all living cells in a board" $ do
        let board = setLiveCells [ (0, 0), (1, 1), (4, 5), (3, 2), (4, 9) ] (unsafeNewBoard (5, 10))
            allDead = resetBoard board
        any isAlive (allCells allDead) `shouldBe` False

emptyBoardTests :: Spec
emptyBoardTests = describe "Empty ConwayBoard" $ do
    let defaultBoard = unsafeNewBoard (5,5)
    it "should be initialized with only dead cells" $ 
        any isAlive defaultBoard `shouldBe` False
    it "should have the correct length" $
        F.length defaultBoard `shouldBe` 25
    it "should return Nothing for negative indexes" $ do
        atPosition (-1,  0 ) defaultBoard `shouldBe` Nothing
        atPosition ( 0, -1 ) defaultBoard `shouldBe` Nothing
        atPosition ( 0,  5 ) defaultBoard `shouldBe` Nothing
        atPosition ( 5,  0 ) defaultBoard `shouldBe` Nothing

populatedBoardTests :: Spec
populatedBoardTests = describe "Populated ConwayBoard" $ do
    let board = setLiveCells [ (0, 0), (1, 1) ] (unsafeNewBoard (2, 2))
        atPositionB = (`atPosition` board)
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
    let b = setLiveCells    [ (2, 0)
                            , (1, 1)
                            , (2, 1)
                            , (1, 2) ] (unsafeNewBoard (3, 3))
    it "should return the correct count of live neighbours" $ do
        countLiveNeighbours (0, 0) b `shouldBe` 1
        countLiveNeighbours (0, 2) b `shouldBe` 2
        countLiveNeighbours (2, 1) b `shouldBe` 3



blockStillLifeTest :: Spec
blockStillLifeTest = describe "Should behave like a still block (4 cells)" $
    it "should remain constant through iterations" $ do
        let seed = setLiveCells [(1, 1), (1, 2), (2, 1), (2, 2)] (unsafeNewBoard (3, 3))
            snapshots = scanr ($) seed (replicate 10 tickBoard)
        all (== seed) snapshots `shouldBe` True

minimalBlockStillLife :: Spec
minimalBlockStillLife = describe "Should behave like a still block (4 cells)" $
    it "should remain constant through iterations" $ do
        let seed = setLiveCells [(0, 0), (0, 1), (1, 0), (1, 1)] (unsafeNewBoard (2, 2))
            snapshots = scanr ($) seed (replicate 10 tickBoard)
        all (== seed) snapshots `shouldBe` True

heterogenousBlockStillLife :: Spec
heterogenousBlockStillLife = describe "Should behave like a still block (4 cells)" $
    it "should remain constant through iterations" $ do
        let seed = setLiveCells [(0, 0), (0, 1), (1, 0), (1, 1)] (unsafeNewBoard (3, 2))
            snapshots = scanr ($) seed (replicate 10 tickBoard)
        all (== seed) snapshots `shouldBe` True

singleCellShouldDieTest :: Spec
singleCellShouldDieTest = describe "A single live cell should die" $ do
    let board = setLiveCells [(2, 2)] (unsafeNewBoard (5, 5))
    it "should return a board with only dead cells" $
            tickBoard board `shouldBe` unsafeNewBoard (5,5)

twoCellsDieTest :: Spec
twoCellsDieTest = describe "Both cells should die as if cause by underpopulation" $ do
    let board = setLiveCells [(0, 0), (1, 1)] (unsafeNewBoard (5, 5))
    it "should return a board with only dead cells" $
            allCells (tickBoard board) `shouldBe` replicate 25 deadCell

flattenSquareBoardTest :: Spec
flattenSquareBoardTest = describe "Should flatten a square ConwayBoard" $
    it "should flatten a board" $ do
        let board = setLiveCells [(0, 0), (1, 1)] (unsafeNewBoard (2, 2))
        positionCellMapping board `shouldBe` [
            ((0, 0), liveCell), 
            ((1, 0), deadCell),
            ((0, 1), deadCell),
            ((1, 1), liveCell)]


blinkerOscillatorTest :: Spec
blinkerOscillatorTest = describe "Should behave like an oscillator with a period of 2" $ do
    let blinkFst = [(2, 1), (2, 2), (2, 3)] -- Vertical
        blinkSnd = [(1, 2), (2, 2), (3, 2)] -- Horizontal
    it "should turn the vertical arrow for a horizontal one" $ do
        let seed = setLiveCells blinkFst (unsafeNewBoard (5, 5))
        let fstIteration = tickBoard seed
        fstIteration `shouldBe` setLiveCells blinkSnd (unsafeNewBoard (5, 5))
    it "should turn the horizontal line into a vertical one" $ do
        let seed = setLiveCells blinkSnd (unsafeNewBoard (5, 5))
            fstIteration = tickBoard seed
        fstIteration `shouldBe` setLiveCells blinkFst (unsafeNewBoard (5, 5))

unsafeNewBoard :: BoardSize -> Board
unsafeNewBoard = fromJust . newBoard