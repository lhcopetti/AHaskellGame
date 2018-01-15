module Component.Behavior.MousePointerBehavior
    ( mousePointer
    , mouseFollower
    ) where

import Control.Monad.Reader (asks)

import Component.Behavior.Behavior (BehaviorType)
import GameEnv (GameEnvironment (..))

import Component.Position (setPosition, getPosition)
import Component.PhysicsClass (setVelocity, getVelocity)
import Input.Mouse (MouseInput (..))
import Vec2.Vec2Behavior (direction)
import Vec2.Vec2Math (addVec2f)

mousePointer :: BehaviorType
mousePointer obj = do
    mousePosition <- asks (position . input)
    return (setPosition obj mousePosition)

mouseFollower :: BehaviorType
mouseFollower obj = do
    -- Physics update
    let pos = getPosition obj
    let vel = getVelocity obj
    let newObj = setPosition obj (addVec2f pos vel)

    mousePosition <- asks (position . input)
    let objPos = getPosition newObj
    return (setVelocity newObj (direction mousePosition objPos))

