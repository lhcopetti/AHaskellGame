module Vec2.Vec2Instances where

import SFML.System.Vector2 (Vec2f (..))

instance Eq Vec2f where
    (==) (Vec2f x y) (Vec2f x' y') = x == x' && y == y'