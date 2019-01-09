module Vec2.Vec2Behavior 
    ( direction
    , orientation
    ) where

import SFML.System.Vector2 (Vec2f (..))

import Vec2.Vec2Math ((|-|), unitVec2f, angleVec2f)

direction :: Vec2f -> Vec2f -> Vec2f
direction target origin = unitVec2f (target |-| origin)

-- | Returns the positive angle in degrees between the vector and x-axis.
orientation :: Vec2f -> Float
orientation v
    | angle < 0 = angle + 360
    | otherwise = angle
        where
            angle = angleVec2f v
