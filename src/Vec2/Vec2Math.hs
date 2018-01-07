module Vec2.Vec2Math ( zero
                     , addVec2f
                     , v2fToTuple
                     , v2uToTuple
                     ) where

import SFML.System.Vector2

zero :: Vec2f 
zero = Vec2f 0.0 0.0

addVec2f :: Vec2f -> Vec2f -> Vec2f 
addVec2f (Vec2f x y) (Vec2f x' y') = Vec2f (x + x') (y + y')

v2fToTuple :: Vec2f -> (Float, Float)
v2fToTuple (Vec2f x y) = (x, y)

v2uToTuple :: Integral a => Vec2u -> (a, a)
v2uToTuple (Vec2u x y) = (fromIntegral x, fromIntegral y)