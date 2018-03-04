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
    , behaveAllB
    , behaveEveryB
    , addCommandBehaviorB
    , behaveOnKeyPressB
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
import Component.Behavior.HigherOrderBehavior (behaveOnce, behaveBoth, behaveAll, behaveEvery)
import Component.Behavior.CommandBehavior (addCommandBehavior)
import Component.Behavior.InputBehavior (behaveOnKeyPress)

encloseToBoxB :: Behavior
encloseToBoxB = Behavior encloseToBox

encloseByWrapAroundB :: Behavior
encloseByWrapAroundB = Behavior encloseByWrapAround

noopB :: Behavior 
noopB = Behavior noopBehavior

deadManWalkingB :: Behavior
deadManWalkingB = Behavior dieBehavior

mousePositionCopierB :: Behavior 
mousePositionCopierB = Behavior mousePositionCopier

mouseFollowerB :: Behavior
mouseFollowerB = Behavior mouseFollower

rotateB :: Float -> Behavior
rotateB f = Behavior (rotate f)

mousePointerB :: Behavior
mousePointerB = Behavior mousePointer

followPointingMouseB :: Behavior
followPointingMouseB = Behavior followPointingMouse

updatePromptForGOCountB :: String -> Behavior
updatePromptForGOCountB prompt = Behavior (updatePromptForGOCount prompt)

deathByUpdatesB :: Behavior 
deathByUpdatesB = Behavior (deathByUpdates 500)

deathByHitsOnWallB :: Behavior 
deathByHitsOnWallB = Behavior (deathByHitsOnWall 5)

updateTextWithMousePositionB :: Behavior
updateTextWithMousePositionB = Behavior updateTextWithMousePosition

behaveOnceB :: Behavior -> Behavior
behaveOnceB (Behavior behType) = Behavior (behaveOnce behType)

behaveBothB :: Behavior -> Behavior -> Behavior
behaveBothB (Behavior first) (Behavior second) = Behavior (behaveBoth first second)

addChildB :: GameObjectCreation -> Behavior
addChildB child = Behavior (addChildBehavior child)

updateMultipleTextsB :: Behavior
updateMultipleTextsB = Behavior (updateMultipleTexts 0)

behaveAllB :: [BehaviorType] -> Behavior
behaveAllB xs = Behavior (behaveAll xs)

behaveEveryB :: Int -> BehaviorType -> Behavior
behaveEveryB counter beh = Behavior (behaveEvery counter beh)

addCommandBehaviorB :: Command -> Behavior
addCommandBehaviorB comm = Behavior (addCommandBehavior comm)

behaveOnKeyPressB :: KeyCode -> BehaviorType -> Behavior
behaveOnKeyPressB key beh = Behavior (behaveOnKeyPress key beh)