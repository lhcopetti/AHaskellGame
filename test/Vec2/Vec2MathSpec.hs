module Vec2.Vec2MathSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.Runner

import SFML.System.Vector2 (Vec2f (..))
import ExtSFML.SFMLInstances ()

import Vec2.Vec2Math
    
main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec
    
spec :: Spec
spec = describe "testVec2fMath" $ do
        it "should return the size of the vectors" $
            map (sizeVec2f . uncurry Vec2f . fst) testCases `shouldBe` map snd testCases
        it "should perform vector subtraction axis-wise" $
            map (uncurry subtractVec2f . fst) subtractTestCases `shouldBe` map snd subtractTestCases
        it "should perform vector division axis-wise" $
            map (uncurry divideVec2f . fst) divideTestCases `shouldBe` map snd divideTestCases
        it "should return the angle in relation to the x axis" $
            map (angleVec2f . fst) angleTestCases `shouldBe` map snd angleTestCases

testCases :: [((Float, Float), Float)]
testCases = [ ((1.0, 1.0), 1.4142135)
            , ((4.0, 3.0), 5.0)
            , ((3.0, 4.0), 5.0)
            , ((0.0, 0.0), 0.0)
            , ((4.0, -3.0), 5.0)
            ]

subtractTestCases :: [((Vec2f, Vec2f), Vec2f)]            
subtractTestCases = [ ((Vec2f 5.0 5.0, Vec2f 5.0 5.0), Vec2f 0.0 0.0)
                    , ((Vec2f 25.0 10.0, Vec2f 20.0 9.0), Vec2f 5.0 1.0)
                    , ((Vec2f 5.0 0.0, Vec2f 10.0 20.0), Vec2f (-5.0) (-20.0))
                    ]

divideTestCases :: [((Vec2f, Vec2f), Vec2f)]
divideTestCases = [ ((Vec2f 5.0 5.0, Vec2f 5.0 5.0), Vec2f 1.0 1.0)
                  , ((Vec2f 25.0 10.0, Vec2f 5.0 2.0), Vec2f 5.0 5.0)
                  , ((Vec2f 5.0 (-20.0), Vec2f (-10.0) 20.0), Vec2f (-0.5) (-1.0))
                  ]

angleTestCases :: [(Vec2f, Float)]
angleTestCases = [ (Vec2f 0 0, 0)
                 , (Vec2f 5 5, 45)
                 , (Vec2f (-5) 5, 135)
                 , (Vec2f (-5) (-5), -135)
                 , (Vec2f 5 (-5), -45)
                 ]