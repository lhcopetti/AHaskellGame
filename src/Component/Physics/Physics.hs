module Component.Physics.Physics
    ( Physics (..)
    ) where

import SFML.System.Vector2 (Vec2f (..))

import Vec2.Vec2Math (minVec2f)
import Component.Physics.PhysicsClass

data Physics = Physics  { velocity :: Vec2f
                        , maxVelocity :: Float
                        }

instance PhysicsClass Physics where
    getVelocity = velocity
    setVelocity phy vel = phy { velocity = minVec2f vel (maxVelocity phy) }