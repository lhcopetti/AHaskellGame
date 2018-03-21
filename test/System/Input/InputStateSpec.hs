module System.Input.InputStateSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.Runner

import System.Input.InputState

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = stepStateTests

stepStateTests :: Spec
stepStateTests = describe "Step Snapshot tests" $ do
        let allTrue = State { justPressed = True, isPressed = True }
        it "should aknowledge the left button press" $
            stepState emptyState Pressed `shouldBe` State True True
        it "a press event while in pressed state should not trigger just pressed" $ do
            let input = State { justPressed = False, isPressed = True }
                output = State { justPressed = False, isPressed = True }
            stepState input Pressed `shouldBe` output
        it "the just pressed should last only one step" $
            stepState allTrue Nil `shouldBe` 
            State { justPressed = False, isPressed = True }
        it "a release event should empty the left mouse snapshot (all false)" $
            stepState allTrue Released `shouldBe` emptyState