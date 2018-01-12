module Component.Behavior.EnclosedBehavior
    ( encloseToBox
    ) where

import SFML.System.Vector2 (Vec2f)

import Control.Monad.Reader (Reader, asks)

import Behavior.BoxedBehavior (boundToDimension)
import Vec2.Vec2Math (addVec2f)
import Component.Behavior.Behavior
import Component.Position
import Component.Physics
import GameEnv


encloseToBox :: BehaviorType
encloseToBox obj = do
        let pos = getPosition obj
        let vel = getVelocity obj

        -- Physics update
        let newObj = setPosition obj (addVec2f pos vel)

        -- Behavior update
        dimension <- asks gameArea
        let newObject = boundToDimension newObj dimension

        -- Return the new object
        return newObject