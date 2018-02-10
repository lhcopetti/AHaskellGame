module Component.Physics.PhysicsClass
    where

import SFML.System.Vector2 (Vec2f)

class PhysicsClass a where 
    getVelocity :: a -> Vec2f
    setVelocity :: a -> Vec2f -> a

    updatePhysics :: a -> IO a