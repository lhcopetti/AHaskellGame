module Component.Behavior.MousePointerBehavior
    ( mousePositionCopier
    , mouseFollower
    , mousePointer
    ) where

import Control.Monad.Reader (asks)

import Component.Behavior.Behavior (BehaviorType)
import GameObject.GameObject ()
import GameEnv (GameEnvironment (..))

import Component.Position (setPosition, getPosition, setRotation)
import Component.Physics.PhysicsClass (setVelocity, getVelocity)
import Input.Mouse (MouseInput (..))
import Vec2.Vec2Behavior (direction, orientation)
import Vec2.Vec2Math (addVec2f, subtractVec2f)

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