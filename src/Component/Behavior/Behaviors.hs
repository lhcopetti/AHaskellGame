module Component.Behavior.Behaviors
    ( encloseToBoxB
    , encloseByWrapAroundB
    , noopB
    , deadManWalkingB
    , mousePositionCopierB
    , mouseFollowerB
    , rotateB
    , mousePointerB
    , followPointingMouseB
    , updatePromptForGOCountB
    , deathByUpdatesB
    , deathByHitsOnWallB
    , updateTextWithMousePositionB
    , behaveOnceB
    , behaveBothB
    , addChildB
    , updateMultipleTextsB
    , behaveSequenceB
    , behaveEveryB
    , addCommandBehaviorB
    , behaveOnKeyPressB
    , behaveOnKeyJustPressedB
    , chooseBehaviorB
    , behaveAllB
    ) where

import SFML.Window.Keyboard (KeyCode)

import GameObject.GameObjectTypes (GameObjectCreation, Command)
import Component.Behavior.Behavior
import Component.Behavior.EnclosedBehavior (encloseToBox, encloseByWrapAround)
import Component.Behavior.MousePointerBehavior (mousePositionCopier, mouseFollower, mousePointer, followPointingMouse)
import Component.Behavior.RotationalBehavior (rotate)
import Component.Behavior.TextBehavior (updatePromptForGOCount, updateTextWithMousePosition, updateMultipleTexts)
import Component.Behavior.NoopBehavior (noopBehavior)
import Component.Behavior.DeathBehavior (dieBehavior, deathByUpdates, deathByHitsOnWall)
import Component.Behavior.ChildBearerBehavior (addChildBehavior)
import Component.Behavior.HigherOrderBehavior (behaveOnce, behaveBoth, behaveSequence, behaveEvery, chooseBehavior, behaveAll)
import Component.Behavior.CommandBehavior (addCommandBehavior)
import Component.Behavior.InputBehavior (behaveOnKeyPress, behaveOnKeyJustPressed)

encloseToBoxB :: Behavior st
encloseToBoxB = Behavior encloseToBox

encloseByWrapAroundB :: Behavior st
encloseByWrapAroundB = Behavior encloseByWrapAround

noopB :: Behavior st 
noopB = Behavior noopBehavior

deadManWalkingB :: Behavior st
deadManWalkingB = Behavior dieBehavior

mousePositionCopierB :: Behavior st 
mousePositionCopierB = Behavior mousePositionCopier

mouseFollowerB :: Behavior st
mouseFollowerB = Behavior mouseFollower

rotateB :: Float -> Behavior st
rotateB f = Behavior (rotate f)

mousePointerB :: Behavior st
mousePointerB = Behavior mousePointer

followPointingMouseB :: Behavior st
followPointingMouseB = Behavior followPointingMouse

updatePromptForGOCountB :: String -> Behavior st
updatePromptForGOCountB prompt = Behavior (updatePromptForGOCount prompt)

deathByUpdatesB :: Behavior st 
deathByUpdatesB = Behavior (deathByUpdates 500)

deathByHitsOnWallB :: Behavior st 
deathByHitsOnWallB = Behavior (deathByHitsOnWall 5)

updateTextWithMousePositionB :: Behavior st
updateTextWithMousePositionB = Behavior updateTextWithMousePosition

behaveOnceB :: Behavior st -> Behavior st
behaveOnceB (Behavior behType) = Behavior (behaveOnce behType)

behaveBothB :: Behavior st -> Behavior st -> Behavior st
behaveBothB (Behavior first) (Behavior second) = Behavior (behaveBoth first second)

addChildB :: GameObjectCreation st -> Behavior st
addChildB child = Behavior (addChildBehavior child)

updateMultipleTextsB :: Behavior st
updateMultipleTextsB = Behavior (updateMultipleTexts 0)

behaveSequenceB :: [BehaviorType st] -> Behavior st
behaveSequenceB xs = Behavior (behaveSequence xs)

behaveEveryB :: Int -> BehaviorType st -> Behavior st
behaveEveryB counter beh = Behavior (behaveEvery counter beh)

addCommandBehaviorB :: Command st -> Behavior st
addCommandBehaviorB comm = Behavior (addCommandBehavior comm)

behaveOnKeyPressB :: KeyCode -> BehaviorType st -> Behavior st
behaveOnKeyPressB key beh = Behavior (behaveOnKeyPress key beh)

behaveOnKeyJustPressedB :: KeyCode -> BehaviorType st -> Behavior st
behaveOnKeyJustPressedB key beh = Behavior (behaveOnKeyJustPressed key beh)

chooseBehaviorB :: Bool -> Behavior st -> Behavior st -> Behavior st
chooseBehaviorB pred (Behavior first) (Behavior second) = Behavior (chooseBehavior pred first second)

behaveAllB :: [Behavior st] -> Behavior st
behaveAllB = Behavior . behaveAll . map behave