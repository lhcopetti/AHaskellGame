module Component.Behavior.EnclosedBehavior
    ( encloseToBox
    , encloseByWrapAround
    ) where

import SFML.System.Vector2 (Vec2f)

import Control.Monad.Reader (Reader, asks)

import Behavior.BoxedBehavior (boundToDimension, wrapAround)
import Vec2.Vec2Math (addVec2f)
import Component.Behavior.Behavior
import Component.Position
import Component.Physics.PhysicsClass
import GameObject.GameObject ()
import GameEnv


encloseToBox :: BehaviorType
encloseToBox obj = do
        dimension <- asks gameArea
        return (boundToDimension obj dimension)

encloseByWrapAround :: BehaviorType
encloseByWrapAround obj = do
    dimension <- asks gameArea
    return (wrapAround obj dimension)