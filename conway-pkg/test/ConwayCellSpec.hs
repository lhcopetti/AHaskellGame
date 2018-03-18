module ConwayCellSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.Runner

import ConwayCell

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = do
    stepDeadCellTests
    stepLiveCellTests
    toggleTests

stepDeadCellTests :: Spec
stepDeadCellTests = describe "Stepping a dead cell" $ do
    it "should keep the cells dead with fewer than 3 neighbours" $
        map (`stepDeadCell` deadCell) [0, 1, 2] `shouldBe` replicate 3 deadCell
    it "should ressucitate a cell with exactly 3 neighbours" $
        stepDeadCell 3 deadCell `shouldBe` liveCell
    it "should keep the cells dead with more than 3 neighbours" $ 
        map (`stepDeadCell` deadCell) [4, 5 .. 8] `shouldBe` replicate 5 deadCell

stepLiveCellTests :: Spec
stepLiveCellTests = describe "Stepping a living cell" $ do
    it "should kill cells with fewer than 2 neighbours" $
        map (`stepLiveCell` liveCell) [0, 1] `shouldBe` replicate 2 deadCell
    it "should keep cells alive with 2 or 3 neighbours" $
        map (`stepLiveCell` liveCell) [2, 3] `shouldBe` replicate 2 liveCell
    it "should kill cells with more then 3 neighbours" $
        map (`stepLiveCell` liveCell) [4, 5 .. 8] `shouldBe` replicate 5 deadCell

toggleTests :: Spec
toggleTests = describe "Toggle cell tests" $ do
    it "should kill living cells" $
        toggle liveCell `shouldBe` deadCell
    it "should reanimate dead cells" $
        toggle deadCell `shouldBe` liveCell