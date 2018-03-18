module System.MouseSnapshotSpec
    ( main
    , spec
    ) where

import SFML.Window.Event
import SFML.Window.Mouse

import Test.Hspec
import Test.Hspec.Runner

import System.MouseSnapshot
import qualified System.InputState as I
    
main :: IO ()
main = hspecWith defaultConfig { configFastFail = True } spec
    
spec :: Spec
spec = stepMouseStateTests

leftClick :: SFEvent
leftClick    = SFEvtMouseButtonPressed  MouseLeft  0 0

leftRelease :: SFEvent
leftRelease  = SFEvtMouseButtonReleased MouseLeft  0 0

rightClick :: SFEvent
rightClick   = SFEvtMouseButtonPressed  MouseRight 0 0

rightRelease :: SFEvent
rightRelease = SFEvtMouseButtonReleased MouseRight 0 0

stepMouseStateTests :: Spec
stepMouseStateTests = describe "Step Mouse Snapshot tests" $ do
    it "should aknowledge the left button press" $
        stepMouseSnapshot emptySnapshot [leftClick] `shouldBe` MouseSnapshot { left = I.State True True }
    it "a release event should empty the left mouse snapshot (all false)" $ do
        let snap = MouseSnapshot { left = I.State True True }
        stepMouseSnapshot snap [leftRelease] `shouldBe` emptySnapshot
    it "a Nil event should keep the snapshot intact (except for justPressed)" $ do
        let snap = MouseSnapshot { left = I.State True False }
        stepMouseSnapshot snap [rightClick, rightRelease] `shouldBe` emptySnapshot
    it "a Nil event should keep the snapshot intact (except for justPressed)" $ do
        let snap = MouseSnapshot { left = I.State True True }
            output = MouseSnapshot { left = I.State False True }
        stepMouseSnapshot snap [rightRelease, rightClick] `shouldBe` output