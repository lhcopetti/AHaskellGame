module System.Input.KeyboardSnapshotSpec
    ( main
    , spec
    ) where

import SFML.Window.Event
import SFML.Window.Keyboard

import qualified Data.Map as M

import Test.Hspec
import Test.Hspec.Runner

import System.Input.KeyboardSnapshot
import qualified System.Input.InputState as I

main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec

spec :: Spec
spec = do
    toKeyEventTest
    reduceKeyboardEvents
    unifyMapsTests
    stepKeyboardTests

qPress :: SFEvent
qPress    = SFEvtKeyPressed KeyQ False False False False

qRelease :: SFEvent
qRelease  = SFEvtKeyReleased KeyQ False False False False

kPress :: SFEvent
kPress = SFEvtKeyPressed KeyK False False False False

kRelease :: SFEvent
kRelease = SFEvtKeyReleased KeyK False False False False

unrelatedEvent :: SFEvent
unrelatedEvent = SFEvtJoystickConnected 0

unrelatedEvent' :: SFEvent
unrelatedEvent' = SFEvtLostFocus


toKeyEventTest :: Spec
toKeyEventTest = describe "Tests the reduction of SFEvents to I.Event" $ do
    it "should return nothing for unrelated events" $ do
        toKeyEvent unrelatedEvent `shouldBe` Nothing
        toKeyEvent unrelatedEvent' `shouldBe` Nothing
    it "should correctly map the key and event for a SFEvent" $ do
        toKeyEvent qPress `shouldBe` Just (KeyQ, I.Pressed)
        toKeyEvent kRelease `shouldBe` Just (KeyK, I.Released)

reduceKeyboardEvents :: Spec
reduceKeyboardEvents = do
    it "should return an empty map for empty event list" $
        reduceEvents [] `shouldBe` M.empty
    it "should ignore unrelated events" $
        reduceEvents [unrelatedEvent, unrelatedEvent'] `shouldBe` M.empty
    it "should correctly map a SFEvent to a key + event" $
        reduceEvents [qPress] `shouldBe` M.singleton KeyQ [I.Pressed]
    it "should process events in the correct order" $
        reduceEvents [qPress, qRelease] `shouldBe` M.singleton KeyQ [I.Pressed, I.Released]
    it "should combine different keys correctly" $ do
        let expected = M.fromList [(KeyQ, [I.Pressed]), (KeyK, [I.Pressed, I.Released])]
        reduceEvents [kPress, qPress, kRelease] `shouldBe` expected

unifyMapsTests :: Spec
unifyMapsTests = describe "Unify keyboard map tests" $ do
    it "empty maps should result in an empty map" $
        unifyMaps M.empty M.empty `shouldBe` M.empty
    it "entries on the first map should be kept" $ do
        let firstMap = M.singleton KeyQ I.emptyState
        unifyMaps firstMap M.empty `shouldBe` firstMap
    it "entries on the second should be added to the first" $ do
        let sndMap = M.singleton KeyQ [I.Pressed]
        unifyMaps M.empty sndMap `shouldBe` M.singleton KeyQ I.emptyState
    it "existing entries on the first map should NOT be overridden" $ do
        let fstMap = M.singleton KeyQ (I.State True True)
        let sndMap = M.singleton KeyQ [I.Released]
        unifyMaps fstMap sndMap `shouldBe` fstMap

stepKeyboardTests :: Spec
stepKeyboardTests = describe "Testing the keyboard snapshot" $ do
    it "should remain constant for empty event list" $
        stepKeyboard M.empty [] `shouldBe` M.empty
    it "should add states for new events" $ do
        let input = M.singleton KeyQ I.emptyState
            output = M.singleton KeyQ (I.State True True)
        stepKeyboard input [qPress] `shouldBe` output
    it "should add entries for new events and keep the old ones" $ do
        let qData = (KeyQ, I.emptyState { I.isPressed = True })
        let input = uncurry M.singleton qData
            output = M.fromList [qData, (KeyK, I.State True True)]
        stepKeyboard input [kPress] `shouldBe` output
    it "should step justPressed entries for single key" $ do
        let input  = M.singleton KeyQ (I.State True True)
            output = M.singleton KeyQ (I.emptyState { I.isPressed = True })
        stepKeyboard input [] `shouldBe` output
    it "should step justPressed entries for all keys" $ do
        let input  = M.fromList [(KeyQ, I.State True True), (KeyK, I.State True True)]
            output = M.fromList [(KeyQ, I.emptyState { I.isPressed = True }), (KeyK, I.emptyState { I.isPressed = True })]
        stepKeyboard input [] `shouldBe` output