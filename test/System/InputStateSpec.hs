module System.InputStateSpec
    ( main
    , spec
    ) where

import SFML.Window.Event
import SFML.Window.Mouse

import Test.Hspec
import Test.Hspec.Runner

import System.InputState
    
main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec
    
spec :: Spec
spec = do
    reduceTests
    stepStateTests

leftClick :: SFEvent
leftClick    = SFEvtMouseButtonPressed  MouseLeft  0 0

leftRelease :: SFEvent
leftRelease  = SFEvtMouseButtonReleased MouseLeft  0 0

rightClick :: SFEvent
rightClick   = SFEvtMouseButtonPressed  MouseRight 0 0

rightRelease :: SFEvent
rightRelease = SFEvtMouseButtonReleased MouseRight 0 0

stepStateTests :: Spec
stepStateTests = describe "Step Snapshot tests" $ do
        let allTrue = State { justPressed = True, isPressed = True }
        it "should aknowledge the left button press" $
            stepState emptyState Pressed `shouldBe` State True True
        it "the just pressed should last only one step" $
            stepState allTrue Nil `shouldBe` 
            State { justPressed = False, isPressed = True }
        it "a release event should empty the left mouse snapshot (all false)" $
            stepState allTrue Released `shouldBe` emptyState

reduceTests :: Spec
reduceTests = do
    it "should return False for empty event list" $
        reduceEvents [] `shouldBe` Nil
    it "should aknowledge the left mouse press" $
        reduceEvents [leftClick] `shouldBe` Pressed
    it "should ignore unrelated events" $
        reduceEvents [rightClick, rightRelease, rightClick] `shouldBe` Nil
    it "left press then left release events should reduce to RELEASE false" $
        reduceEvents [leftClick, leftRelease] `shouldBe` Released
    it "left release then left press events should return true" $
        reduceEvents [leftRelease, leftClick] `shouldBe` Pressed
    it "should not depend on being the last value of the event list" $
        reduceEvents [leftClick, leftRelease, leftClick, rightRelease] `shouldBe` Pressed
    it "should not depend on being the last value of the event list" $
        reduceEvents [leftClick, leftRelease, leftClick, leftRelease, rightRelease] `shouldBe` Released