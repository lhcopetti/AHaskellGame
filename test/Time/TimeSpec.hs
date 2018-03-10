module Time.TimeSpec
    ( main
    , spec
    ) where

import Data.Time.Clock
import Time.Time

import Test.Hspec
import Test.Hspec.Runner
    
main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec
    
spec :: Spec
spec = describe "testTimeModule" $ do
        it "should return all '-' for negative values" $
            formatSeconds (-10) `shouldBe` "--:--:--"
        it "should return the seconds formatted" $
            formatSeconds 10 `shouldBe` "00:00:10"
        it "should overflow to minutes" $
            formatSeconds 60 `shouldBe` "00:01:00"
        it "should format minutes" $
            formatSeconds (5 * 60 + 10) `shouldBe` "00:05:10"
        it "should overflow to hours" $
            formatSeconds (60 * 60) `shouldBe` "01:00:00"
        it "should format hours" $
            formatSeconds (2 * 60 * 60 + 90) `shouldBe` "02:01:30"
        it "should grow the hour slot unconditionally" $ do
            formatSeconds (28 * 60 * 60) `shouldBe` "28:00:00"
            formatSeconds (128 * 60 * 60) `shouldBe` "128:00:00"