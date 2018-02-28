module Component.Behavior.MousePointerBehavior
    ( mousePositionCopier
    , mouseFollower
    , mousePointer
    , followPointingMouse
    , mouseDistance
    ) where

import Control.Monad.Reader (Reader, asks)
import Control.Monad.Trans.State

import Component.Behavior.Behavior (BehaviorType)
import Component.Behavior.HigherOrderBehavior (behaveBoth)
import GameObject.GameObject (GameObject (..))
import GameEnv (GameEnvironment (..))

import Component.Position (setPosition, getPosition, setRotation)
import Component.Physics.PhysicsClass (setVelocity)
import Component.Physics.Physics ()
import Input.Mouse (MouseInput (..))
import Vec2.Vec2Behavior (direction, orientation)
import Vec2.Vec2Math (subtractVec2f, distanceVec2f)

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

mouseDistance :: GameObject -> StateT Int (Reader GameEnvironment) Float
mouseDistance obj = do
    mousePosition <- asks (mousePos . input)
    let objPosition = getPosition obj
    return (distanceVec2f mousePosition objPosition)