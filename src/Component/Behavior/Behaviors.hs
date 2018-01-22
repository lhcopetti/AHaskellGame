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
    , followsAndDiesCloseToMouseB
    ) where

import GameObject.GameObjectTypes (GameObjectCreation)

import Component.Behavior.Behavior
import Component.Behavior.EnclosedBehavior (encloseToBox, encloseByWrapAround)
import Component.Behavior.MousePointerBehavior (mousePositionCopier, mouseFollower, mousePointer, followPointingMouse, followsAndDiesCloseToMouse)
import Component.Behavior.RotationalBehavior (rotate)
import Component.Behavior.TextBehavior (updatePromptForGOCount, updateTextWithMousePosition)
import Component.Behavior.NoopBehavior (noopBehavior)
import Component.Behavior.DeathBehavior (dieBehavior, deathByUpdates, deathByHitsOnWall)
import Component.Behavior.ChildBearerBehavior (addChildBehavior)
import Component.Behavior.HigherOrderBehavior (behaveOnce, behaveBoth)

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
behaveBothB (Behavior fst) (Behavior snd) = Behavior (behaveBoth fst snd)

addChildB :: GameObjectCreation -> Behavior
addChildB child = Behavior (addChildBehavior child)

followsAndDiesCloseToMouseB :: Behavior
followsAndDiesCloseToMouseB = Behavior followsAndDiesCloseToMouse