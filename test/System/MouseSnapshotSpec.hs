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

stepSnapshotTests :: Spec
stepSnapshotTests = describe "Step Mouse Snapshot test" $ do
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
    let leftClick =     SFEvtMouseButtonPressed  MouseLeft  0 0
        leftRelease =   SFEvtMouseButtonReleased MouseLeft  0 0
        rightClick =    SFEvtMouseButtonPressed  MouseRight 0 0
        rightRelease =  SFEvtMouseButtonReleased MouseRight 0 0
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


-- isLeftPressedTests :: Spec
-- isLeftPressedTests = describe "Mouse Event tests" $ do
--         let leftClick =     SFEvtMouseButtonPressed  MouseLeft  0 0
--             leftRelease =   SFEvtMouseButtonReleased MouseLeft  0 0
--             rightClick =    SFEvtMouseButtonPressed  MouseRight 0 0
--             rightRelease =  SFEvtMouseButtonReleased MouseRight 0 0
--         it "should return False for empty event list" $
--             isLeftPressed [] `shouldBe` False
--         it "left click events should return true" $
--             isLeftPressed [leftClick] `shouldBe` True
--         it "right click events should be ignored" $
--             isLeftPressed [rightClick] `shouldBe` False
--         it "left press then left release events should return false" $
--             isLeftPressed [leftClick, leftRelease] `shouldBe` False
--         it "left release then left press events should return true" $
--             isLeftPressed [leftRelease, leftClick] `shouldBe` True
--         it "should not depend on being the last value of the event list" $
--             isLeftPressed [leftClick, leftRelease, leftClick, rightRelease] `shouldBe` True
--         it "should not depend on being the last value of the event list" $
--             isLeftPressed [leftClick, leftRelease, leftClick, leftRelease, rightRelease] `shouldBe` False