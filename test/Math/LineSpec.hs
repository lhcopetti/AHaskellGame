module Math.LineSpec
    ( main
    , spec
    ) where

import SFML.System.Vector2 (Vec2f (..))
import SFML.Graphics.Rect  (FloatRect (..))
import ExtSFML.SFMLInstances ()


import Test.Hspec
import Test.Hspec.Runner

import Math.Line (getRectFromLine)
    
main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec
    
spec :: Spec
spec = describe "testGetRectFromLine" $
       it "should return the rect that correctly wraps the line" $
       map (uncurry getRectFromLine . fst) testCases `shouldBe` map snd testCases

testCases :: [(((Vec2f, Vec2f), Float), [Vec2f])]
testCases = [ ((( Vec2f 0 1, Vec2f 5 0), 3), [Vec2f 0 1.5, Vec2f 5 1.5, Vec2f 5 (-1.5), Vec2f 0 (-1.5)])
            , ((( Vec2f 10 10, Vec2f 10 20), 1), [Vec2f 9.5 10, Vec2f 9.5 20, Vec2f 10.5 20, Vec2f 10.5 10])
            ]