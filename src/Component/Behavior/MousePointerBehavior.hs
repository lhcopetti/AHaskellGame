module Component.Behavior.MousePointerBehavior
    ( mousePositionCopier
    , mouseFollower
    , mousePointer
    , followPointingMouse
    , mouseDistance
    ) where

import Control.Monad.Reader (asks)

import Component.Behavior.Behavior (BehaviorType)
import Component.Behavior.HigherOrderBehavior (behaveBoth)
import GameObject.GameObject (GameObject (..))
import GameEnv (GameEnvironment (..))
import Updatable (UpdateMStack)

import Component.Position (setPosition, getPosition, setRotation)
import Component.Physics.PhysicsClass (setVelocity)
import Component.Physics.Physics ()
import Input.Mouse (MouseInput (..))
import Vec2.Vec2Behavior (direction, orientation)
import Vec2.Vec2Math ((|-|), distanceVec2f)

mousePositionCopier :: BehaviorType st
mousePositionCopier obj = do
    mousePosition <- asks (mousePos . input)
    return (setPosition obj mousePosition)

mouseFollower :: BehaviorType st
mouseFollower obj = do
    mousePosition <- asks (mousePos . input)
    let objPos = getPosition obj
    return (setVelocity obj (direction mousePosition objPos))

mousePointer :: BehaviorType st 
mousePointer obj = do
    mousePosition <- asks (mousePos . input)
    let objPos = getPosition obj
    let angle = orientation (mousePosition |-| objPos)
    return (setRotation angle obj)

followPointingMouse :: BehaviorType st
followPointingMouse = behaveBoth mousePointer mouseFollower

mouseDistance :: GameObject st -> UpdateMStack Float st
mouseDistance obj = do
    mousePosition <- asks (mousePos . input)
    let objPosition = getPosition obj
    return (distanceVec2f mousePosition objPosition)
