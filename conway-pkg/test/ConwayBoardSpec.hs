module ConwayBoardSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.Runner
    
main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec
    
spec :: Spec
spec = describe "Simple test" $
       it "fail" $ 0 `shouldBe` 0
