module Component.Behavior.MousePointerBehavior
    ( mousePositionCopier
    , mouseFollower
    , mousePointer
    , followPointingMouse
    , followsAndDiesCloseToMouse
    ) where

import Control.Monad.Reader (Reader, asks)

import Component.Behavior.Behavior (BehaviorType)
import Component.Behavior.HigherOrderBehavior (behaveBoth, behaviorPred)
import Component.Behavior.DeathBehavior (dieBehavior)
import GameObject.GameObject (GameObject (..))
import GameEnv (GameEnvironment (..))

import Component.Position (setPosition, getPosition, setRotation)
import Component.Physics.PhysicsClass (setVelocity, getVelocity)
import Input.Mouse (MouseInput (..))
import Vec2.Vec2Behavior (direction, orientation)
import Vec2.Vec2Math (addVec2f, subtractVec2f, distanceVec2f)

mousePositionCopier :: BehaviorType
mousePositionCopier obj = do
    mousePosition <- asks (mousePos . input)
    return (setPosition obj mousePosition)

mouseFollower :: BehaviorType
mouseFollower obj = do
    mousePosition <- asks (mousePos . input)
    let objPos = getPosition obj
    return (setVelocity obj (direction mousePosition objPos))

mousePointer :: BehaviorType 
mousePointer obj = do
    mousePosition <- asks (mousePos . input)
    let objPos = getPosition obj
    let angle = orientation (subtractVec2f mousePosition objPos)
    return (setRotation angle obj)

followPointingMouse :: BehaviorType
followPointingMouse = behaveBoth mousePointer mouseFollower

mouseDistance :: GameObject -> Reader GameEnvironment Float
mouseDistance obj = do
    mousePosition <- asks (mousePos . input)
    let objPosition = getPosition obj
    return (distanceVec2f mousePosition objPosition)

followsAndDiesCloseToMouse :: BehaviorType
followsAndDiesCloseToMouse obj = do
    newObj <- followPointingMouse obj
    distanceToMouse <- mouseDistance newObj
    behaviorPred (distanceToMouse < 5.0) dieBehavior followsAndDiesCloseToMouse newObj