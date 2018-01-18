module Vec2.Vec2BehaviorSpec
    ( main
    , spec
    ) where

        
import  SFML.System.Vector2 (Vec2f (..))

import Test.Hspec
import Test.Hspec.Runner

import Vec2.Vec2Behavior
import Vec2.Vec2Math (zero)
import Vec2.Vec2Instances

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = do
    testDirectionVec2f
    testOrientationVec2f


testDirectionVec2f :: Spec
testDirectionVec2f = describe "testDirectionVec2f" $
    it "should return the unit vector direction towards the target" $
        map (uncurry direction . fst) directionTestCases `shouldBe` map snd directionTestCases


directionTestCases :: [((Vec2f, Vec2f), Vec2f)]
directionTestCases = [ ((Vec2f 100 100, zero), Vec2f 0.7071067811865476 0.7071067811865476)
            , ((Vec2f 0 200, Vec2f 0 100),          Vec2f 0.0 1.0)
            , ((Vec2f 200.0 0.0, Vec2f 100.0 0),    Vec2f 1.0 0.0)
            ]

testOrientationVec2f :: Spec
testOrientationVec2f = describe "testOrientationVec2f" $
    it "should corretly compute the angle between the target and origin" $
    map (orientation . fst) orientationTestCases `shouldBe` map snd orientationTestCases


orientationTestCases :: [(Vec2f, Float)]
orientationTestCases = [ (zero, 0)
                       , (Vec2f 5 5, 45)
                       , (Vec2f (-5) 5, 135)
                       , (Vec2f (-5) (-5), 225)
                       , (Vec2f 5 (-5), 315)
                       ]