module Component.Physics.PhysicsFactory
    ( newEmptyPhysics
    , newPhysics
    , newSimplePhysics
    ) where

import SFML.System.Vector2 (Vec2f (..))

import Component.Physics.Physics
import Vec2.Vec2Math (zero)

defaultMaxVelocity :: Float
defaultMaxVelocity = 5.0

newEmptyPhysics :: Physics
newEmptyPhysics = Physics zero defaultMaxVelocity

newPhysics :: Vec2f -> Float -> Physics
newPhysics = Physics

newSimplePhysics :: Vec2f -> Physics
newSimplePhysics vec = Physics vec defaultMaxVelocity