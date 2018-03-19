module System.Input.MouseSnapshotSpec
    ( main
    , spec
    ) where

import SFML.Window.Event
import SFML.Window.Mouse

import Test.Hspec
import Test.Hspec.Runner

import System.Input.MouseSnapshot
import qualified System.Input.InputState as I

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = do
    stepLeftMouseStateTests
    stepRightMouseStateTests
    reduceLeftMouseEventTests

leftClick :: SFEvent
leftClick    = SFEvtMouseButtonPressed  MouseLeft  0 0

leftRelease :: SFEvent
leftRelease  = SFEvtMouseButtonReleased MouseLeft  0 0

rightClick :: SFEvent
rightClick   = SFEvtMouseButtonPressed  MouseRight 0 0

rightRelease :: SFEvent
rightRelease = SFEvtMouseButtonReleased MouseRight 0 0

unrelatedEvent :: SFEvent
unrelatedEvent = SFEvtJoystickConnected 0

unrelatedEvent' :: SFEvent
unrelatedEvent' = SFEvtLostFocus


stepLeftMouseStateTests :: Spec
stepLeftMouseStateTests = describe "Step Left-Mouse Snapshot tests" $ do
    let mkLeftSnap snap = mkMouseSnapshot snap I.emptyState
    it "should aknowledge the left button press" $
        stepMouseSnapshot emptySnapshot [leftClick] `shouldBe` mkLeftSnap (I.State True True)
    it "a release event should empty the left mouse snapshot (all false)" $ do
        let snap = mkLeftSnap (I.State True True)
        stepMouseSnapshot snap [leftRelease] `shouldBe` emptySnapshot
    it "a Nil event should keep the snapshot intact (except for justPressed)" $ do
        let snap = mkLeftSnap (I.State True False)
        stepMouseSnapshot snap [unrelatedEvent, unrelatedEvent'] `shouldBe` emptySnapshot
    it "a Nil event should keep the snapshot intact (except for justPressed)" $ do
        let snap = mkLeftSnap (I.State True True)
            output = mkLeftSnap (I.State False True)
        stepMouseSnapshot snap [unrelatedEvent', unrelatedEvent] `shouldBe` output

stepRightMouseStateTests :: Spec
stepRightMouseStateTests = describe "Step Right-Mouse Snapshot tests" $ do
    let mkRightSnap = mkMouseSnapshot I.emptyState
    it "should aknowledge the left button press" $
        stepMouseSnapshot emptySnapshot [rightClick] `shouldBe` mkRightSnap (I.State True True)
    it "a release event should empty the left mouse snapshot (all false)" $ do
        let snap = mkRightSnap (I.State True True)
        stepMouseSnapshot snap [rightRelease] `shouldBe` emptySnapshot
    it "a Nil event should keep the snapshot intact (except for justPressed)" $ do
        let snap = mkRightSnap (I.State True False)
        stepMouseSnapshot snap [unrelatedEvent, unrelatedEvent'] `shouldBe` emptySnapshot
    it "a Nil event should keep the snapshot intact (except for justPressed)" $ do
        let snap = mkRightSnap (I.State True True)
            output = mkRightSnap (I.State False True)
        stepMouseSnapshot snap [unrelatedEvent', unrelatedEvent] `shouldBe` output

reduceLeftMouseEventTests :: Spec
reduceLeftMouseEventTests = do
    let reduceLEvents = reduceEvents MouseLeft
    it "should return False for empty event list" $
        reduceLEvents [] `shouldBe` I.Nil
    it "should aknowledge the left mouse press" $
        reduceLEvents [leftClick] `shouldBe` I.Pressed
    it "should ignore unrelated events" $
        reduceLEvents [rightClick, rightRelease, rightClick] `shouldBe` I.Nil
    it "left press then left release events should reduce to RELEASE false" $
        reduceLEvents [leftClick, leftRelease] `shouldBe` I.Released
    it "left release then left press events should return true" $
        reduceLEvents [leftRelease, leftClick] `shouldBe` I.Pressed
    it "should not depend on being the last value of the event list" $
        reduceLEvents [leftClick, leftRelease, leftClick, rightRelease] `shouldBe` I.Pressed
    it "should not depend on being the last value of the event list" $
        reduceLEvents [leftClick, leftRelease, leftClick, leftRelease, rightRelease] `shouldBe` I.Released