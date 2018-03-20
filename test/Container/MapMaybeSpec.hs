module Container.MapMaybeSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.Runner

import qualified Data.Map as M

import Container.MapMaybe
    
main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec
    
spec :: Spec
spec = describe "Should behave like the M.insertWith with maybe values" $ do
        it "should return the same map for Nothing values" $ do
            insertWithMaybe (+) 0 Nothing M.empty `shouldBe` M.empty
            let testMap = M.fromList [(0, 2), (1, 3)]
            insertWithMaybe (+) 0 Nothing testMap `shouldBe` testMap

        it "should insertWith the value even if the map does not contain the key" $
            insertWithMaybe (+) 0 (Just 0) M.empty `shouldBe` M.singleton 0 0

        it "should update the value if the maybe is present (just)" $ do
            let testMap = M.fromList [(0, 2), (1, 3)]
            insertWithMaybe (+) 0 (Just 3) testMap `shouldBe` M.adjust (const 5) 0 testMap