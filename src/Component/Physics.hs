module Component.Physics
    where

import SFML.System.Vector2 (Vec2f)

class Physics a where 
    getVelocity :: a -> Vec2f
    setVelocity :: a -> Vec2f -> a