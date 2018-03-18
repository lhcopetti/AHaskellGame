module System.MouseSnapshotSpec
    ( main
    , spec
    ) where

import SFML.Window.Event
import SFML.Window.Mouse

import Test.Hspec
import Test.Hspec.Runner

import System.MouseSnapshot
    
main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec
    
spec :: Spec
spec = do
    reduceTests
    stepSnapshotTests
    stepMouseSnapshotTests

leftClick :: SFEvent
leftClick    = SFEvtMouseButtonPressed  MouseLeft  0 0

leftRelease :: SFEvent
leftRelease  = SFEvtMouseButtonReleased MouseLeft  0 0

rightClick :: SFEvent
rightClick   = SFEvtMouseButtonPressed  MouseRight 0 0

rightRelease :: SFEvent
rightRelease = SFEvtMouseButtonReleased MouseRight 0 0

stepMouseSnapshotTests :: Spec
stepMouseSnapshotTests = describe "Step Mouse Snapshot tests" $ do
    it "should aknowledge the left button press" $
        stepMouseSnapshot emptySnapshot [leftClick] `shouldBe` MouseSnapshot True True
    it "a release event should empty the left mouse snapshot (all false)" $ do
        let snap = MouseSnapshot { leftIsPressed = True, leftJustPressed = True }
        stepMouseSnapshot snap [leftRelease] `shouldBe` emptySnapshot
    it "a Nil event should keep the snapshot intact (except for justPressed)" $ do
        let snap = MouseSnapshot { leftIsPressed = True, leftJustPressed = False }
        stepMouseSnapshot snap [rightRelease] `shouldBe` snap
    it "a Nil event should keep the snapshot intact (except for justPressed)" $ do
        let snap = MouseSnapshot { leftIsPressed = True, leftJustPressed = True }
        stepMouseSnapshot snap [rightRelease] `shouldBe` snap { leftJustPressed = False }
    
stepSnapshotTests :: Spec
stepSnapshotTests = describe "Step Snapshot tests" $ do
        let allTrue = MouseSnapshot { leftJustPressed = True, leftIsPressed = True }
        it "should aknowledge the left button press" $
            stepSnapshot emptySnapshot Pressed `shouldBe` MouseSnapshot True True
        it "the just pressed should last only one step" $
            stepSnapshot allTrue Nil `shouldBe` 
            MouseSnapshot { leftJustPressed = False, leftIsPressed = True }
        it "a release event should empty the left mouse snapshot (all false)" $
            stepSnapshot allTrue Released `shouldBe` emptySnapshot

reduceTests :: Spec
reduceTests = do
    it "should return False for empty event list" $
        reduceLeftEvents [] `shouldBe` Nil
    it "should aknowledge the left mouse press" $
        reduceLeftEvents [leftClick] `shouldBe` Pressed
    it "should ignore unrelated events" $
        reduceLeftEvents [rightClick, rightRelease, rightClick] `shouldBe` Nil
    it "left press then left release events should reduce to RELEASE false" $
        reduceLeftEvents [leftClick, leftRelease] `shouldBe` Released
    it "left release then left press events should return true" $
        reduceLeftEvents [leftRelease, leftClick] `shouldBe` Pressed
    it "should not depend on being the last value of the event list" $
        reduceLeftEvents [leftClick, leftRelease, leftClick, rightRelease] `shouldBe` Pressed
    it "should not depend on being the last value of the event list" $
        reduceLeftEvents [leftClick, leftRelease, leftClick, leftRelease, rightRelease] `shouldBe` Released