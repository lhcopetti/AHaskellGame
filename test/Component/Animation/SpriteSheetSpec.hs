module Component.Animation.SpriteSheetSpec
    ( main
    , spec
    ) where

import SFML.Graphics.Rect (IntRect (..))

import Test.Hspec
import Test.Hspec.Runner

import Component.Draw.Animation.SpriteSheet (Size (..), Ratio (..), getIntRect)
import GameObject.GameObjectTypes
import ExtSFML.SFMLInstances ()
    
main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec


spec :: Spec
spec = describe "testGetIntRect" $
       it "should return the correct rect given the index, size and ratio" $
       map (uncurry3 getIntRect . fst) testCases `shouldBe` map snd testCases

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

testCases :: [((Size, Ratio, Int), IntRect)]
testCases = [ ((Size 30 20, Ratio 2 5, 0), IntRect 0 0 30 20)
            , ((Size 30 20, Ratio 2 5, 1), IntRect 30 0 30 20)
            , ((Size 30 20, Ratio 2 5, 2), IntRect 0 20 30 20)
            , ((Size 30 20, Ratio 2 5, 9), IntRect 30 (20 * 4) 30 20)
            ]