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
        it "should sum both of the vector components" $
            map (uncurry (|+|) . fst) addTestCases `shouldBe` map snd addTestCases
        it "should return the size of the vectors" $
            map (sizeVec2f . uncurry Vec2f . fst) testCases `shouldBe` map snd testCases
        it "should perform vector subtraction axis-wise" $
            map (uncurry (|-|) . fst) subtractTestCases `shouldBe` map snd subtractTestCases
        it "should perform vector division axis-wise" $
            map (uncurry (|/|) . fst) divideTestCases `shouldBe` map snd divideTestCases
        it "should return the angle in relation to the x axis" $
            map (angleVec2f . fst) angleTestCases `shouldBe` map snd angleTestCases
        it "should return both perpendicular vectors to the one supplied" $
            map (getOrthoVec2f . fst) orthoTestCases `shouldBe` map snd orthoTestCases
        it "should flip the Vec2f components" $ do
            flipV2f (Vec2f 5 5)     `shouldBe` Vec2f 5 5
            flipV2f (Vec2f 5 10)    `shouldBe` Vec2f 10 5
        it "should only modify the X component" $
            map (uncurry onX . fst) onTestCases `shouldBe` map snd onTestCases
        it "should only modify the Y component" $
            map (uncurry onY . fst) onTestCases `shouldBe` map (flipV2f . snd) onTestCases



testCases :: [((Float, Float), Float)]
testCases = [ ((1.0, 1.0), 1.4142135)
            , ((4.0, 3.0), 5.0)
            , ((3.0, 4.0), 5.0)
            , ((0.0, 0.0), 0.0)
            , ((4.0, -3.0), 5.0)
            ]

addTestCases :: [ ((Vec2f, Vec2f), Vec2f) ]
addTestCases =  [ ((Vec2f 5.0 5.0, Vec2f 5.0 5.0), Vec2f 10.0 10.0)
                , ((Vec2f 25.0 10.0, Vec2f 20.0 9.0), Vec2f 45.0 19.0)
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

orthoTestCases :: [(Vec2f, (Vec2f, Vec2f))]
orthoTestCases = [ (Vec2f 0  1, (Vec2f 1 0      , Vec2f (-1) 0  ))
                 , (Vec2f 10 0, (Vec2f 0 (-10)  , Vec2f 0 10    ))
                 , (Vec2f 5  5, (Vec2f 5 (-5)   , Vec2f (-5) 5   ))
                 ]

onTestCases :: [((Float -> Float, Vec2f), Vec2f)]
onTestCases =  [ (((+3), Vec2f 0 0     ), Vec2f 3   0)
                , (((*5), Vec2f 5 5     ), Vec2f 25  5)
                , (((/10), Vec2f 50 50  ), Vec2f 5   50)
                ]
