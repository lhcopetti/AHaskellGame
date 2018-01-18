module Component.Position
    where

import SFML.System.Vector2 (Vec2f (..))

class Position a where
    getPosition :: a -> Vec2f
    setPosition :: a -> Vec2f -> a

    getRotation :: a -> Float
    setRotation :: Float -> a -> a