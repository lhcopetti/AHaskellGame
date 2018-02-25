module ConwayBoardSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.Runner
import Control.Monad.State.Lazy
import Data.Maybe (isJust)

import qualified Data.Foldable as F (length)

import ConwayBoard
import ConwayCell

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = do
    boardCreation

boardCreationTests :: Spec
boardCreationTests = describe "ConwayBoard constructor" $ do
    it "should only allow nonzero and non-negatives dimensions" $ do
        let invalidDimensions = [(0, 3), (3, 0), (0, 0), (-1, 5), (5, -1), (-5, -5)]
        any isJust (newBoard <$> invalidDimensions) `shouldBe` False
    it "should return the correct size" $ do
        let boards = newBoard <$> [(3, 3), (1, 5), (5, 1)]
        map boardSize boards `shouldBe` [(3, 3), (1, 5), (5, 1)]