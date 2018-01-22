module Vec2.Vec2Math ( zero
                     , addVec2f
                     , subtractVec2f
                     , multiplyScalarVec2f
                     , divideVec2f
                     , sizeVec2f
                     , unitVec2f
                     , v2fToTuple
                     , v2uToTuple
                     , minVec2f
                     , angleVec2f
                     , distanceVec2f
                     ) where

import SFML.System.Vector2
import Math.Angle (toDegree)

zero :: Vec2f 
zero = Vec2f 0.0 0.0

addVec2f :: Vec2f -> Vec2f -> Vec2f 
addVec2f (Vec2f x y) (Vec2f x' y') = Vec2f (x + x') (y + y')

subtractVec2f :: Vec2f -> Vec2f -> Vec2f
subtractVec2f (Vec2f x y) (Vec2f x' y') = Vec2f (x - x') (y - y')

divideVec2f :: Vec2f -> Vec2f -> Vec2f
divideVec2f (Vec2f x y) (Vec2f x' y') = Vec2f (x / x') (y / y')

divideScalarVec2f :: Vec2f -> Float -> Vec2f
divideScalarVec2f (Vec2f x y) value = Vec2f (x / value) (y / value)

multiplyScalarVec2f :: Vec2f -> Float -> Vec2f
multiplyScalarVec2f (Vec2f x y) value = Vec2f (x * value) (y * value)

unitVec2f :: Vec2f -> Vec2f
unitVec2f v = divideScalarVec2f v (sizeVec2f v)

sizeVec2f :: Vec2f -> Float
sizeVec2f = sqrt . sizeSquaredVec2f

sizeSquaredVec2f :: Vec2f -> Float
sizeSquaredVec2f (Vec2f x y) = x ^ 2 + y ^ 2

v2fToTuple :: Vec2f -> (Float, Float)
v2fToTuple (Vec2f x y) = (x, y)

v2uToTuple :: Integral a => Vec2u -> (a, a)
v2uToTuple (Vec2u x y) = (fromIntegral x, fromIntegral y)

minVec2f :: Vec2f -> Float -> Vec2f
minVec2f vec maxVel 
    | sizeVec2f vec <= maxVel = vec
    | otherwise = (`multiplyScalarVec2f` maxVel) . unitVec2f $ vec

angleVec2f :: Vec2f -> Float
angleVec2f (Vec2f x y) = toDegree (atan2 y x)

distanceVec2f :: Vec2f -> Vec2f -> Float
distanceVec2f (Vec2f x y) (Vec2f x' y') = sqrt $ (x - x') ^ 2 + (y - y') ^ 2