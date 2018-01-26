module Math.MathFunctionsSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.Runner

import Math.MathFunctions (signumWithoutZero)
    
main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec
    
spec :: Spec
spec = describe "testSignumWithoutZero" $
       it "should return the sign of the number, zeros should be (+1.0) " $
       map (signumWithoutZero . fst) testCases `shouldBe` map snd testCases

testCases :: [(Float, Float)]
testCases = [ (5.0   , 1.0)
                             , (2.5   , 1.0)
                             , (0.33  , 1.0)
                             , (-3.0  , -1.0)
                             , (-0.01 , -1.0)
                             , (0.0   , 1.0)
                             , (-0.0  , 1.0)
                             ]