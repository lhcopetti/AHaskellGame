module Vec2.Vec2Math ( zero
                     , addVec2f
                     ) where

import SFML.System.Vector2

zero :: Vec2f 
zero = Vec2f 0.0 0.0

addVec2f :: Vec2f -> Vec2f -> Vec2f 
addVec2f (Vec2f x y) (Vec2f x' y') = Vec2f (x + x') (y + y')