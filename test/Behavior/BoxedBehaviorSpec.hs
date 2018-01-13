module Behavior.BoxedBehaviorSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.Runner

import Behavior.BoxedBehavior (wrapAroundValue)
    
main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec
    
spec :: Spec
spec = describe "wrapAroundValue" $
       it "should correctly wrap the values around respecting the max parameter" $
       map applyWrapAround testCases `shouldBe` map result testCases

data WrapAroundValueData = WRVD 
                           { value  :: Integer
                           , max    :: Integer
                           , result :: Integer
                           }

testCases :: [WrapAroundValueData]
testCases = [ WRVD 10   300      10
            , WRVD 290  300      290
            , WRVD 310  300      10
            , WRVD 610  300      10
            , WRVD (-10) 300     290
            , WRVD (-320) 300    280
            , WRVD (-100) 300    200
            ]

applyWrapAround :: WrapAroundValueData -> Integer
applyWrapAround (WRVD value max _) = wrapAroundValue value max