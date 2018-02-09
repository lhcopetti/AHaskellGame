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
newEmptyPhysics = SimplePhy zero defaultMaxVelocity

newPhysics :: Vec2f -> Float -> Physics
newPhysics = SimplePhy

newSimplePhysics :: Vec2f -> Physics
newSimplePhysics vec = SimplePhy vec defaultMaxVelocity