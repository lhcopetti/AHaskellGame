module Component.Physics.Physics
    ( Physics (..)
    , updatePhysics
    ) where

import SFML.System.Vector2 (Vec2f (..))

import Vec2.Vec2Math (minVec2f, addVec2f)
import Component.Physics.PhysicsClass
import Component.Position

data Physics = Physics  { velocity :: Vec2f
                        , maxVelocity :: Float
                        }

instance PhysicsClass Physics where
    getVelocity = velocity
    setVelocity phy vel = phy { velocity = minVec2f vel (maxVelocity phy) }

updatePhysics :: (Position a, PhysicsClass a) => a -> a
updatePhysics obj = let
    pos = getPosition obj
    vel = getVelocity obj
    newPos = updatePosition pos vel
    in setPosition obj newPos

updatePosition :: Vec2f -> Vec2f -> Vec2f
updatePosition = addVec2f