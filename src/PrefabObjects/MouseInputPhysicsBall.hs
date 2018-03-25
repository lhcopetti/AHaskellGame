module PrefabObjects.MouseInputPhysicsBall
    ( mkMouseInputPhysicsBall
    ) where

import SFML.System.Vector2

import GameObjectFactory (createGameObject, createSimplePhysicsGO)
import GameObject.GameObjectTypes
import qualified System.Input.MouseSnapshot as M (MButton (..))
import Component.Input.Input
import Component.Draw.Drawing ()
import Component.Draw.TextDrawing (createText)
import Component.Behavior.Behaviors (behaveAllB)
import Component.Behavior.MousePointerBehavior (mousePositionCopier)
import Component.Behavior.CollisionBehavior (onCollision)
import Component.Behavior.DeathBehavior (dieBehavior)
import Vec2.Vec2Math (zero)
import ChildBearer
import Physics.PhysicsTypes
import Physics.CirclePhysics (mkCirclePhysicsD)
import Physics.PhysicsLayer (setLayers)
import System.Messaging.PhysicsInbox

mkMouseInputPhysicsBall :: GameObjectCreation st
mkMouseInputPhysicsBall = do
    text <- createText 15 "Creates physics balls on click"
    let behs = Behavior <$> [mousePositionCopier, createPhysicsBallsOnClick]
    let go = createSimplePhysicsGO text (behaveAllB behs) (Vec2f 0 0) zero
    return go

createPhysicsBallsOnClick :: BehaviorType st
createPhysicsBallsOnClick obj = do
    pressed <- isJustPressed M.MLeft
    mousePos <- mousePosition
    let newObj
            | pressed = addChildP (createPhyBall mousePos) obj
            | otherwise = obj
    return newObj

createPhyBall :: Vec2f -> PhysicsWorld -> GameObjectCreation st
createPhyBall pos world = do
    (phy, drw) <- mkCirclePhysicsD 5.0 pos world
    let beh = Behavior (onCollision dieBehavior)
        msg = PMSG $ setLayers 2
        obj = createGameObject drw beh phy pos
    return (addInbox msg obj)