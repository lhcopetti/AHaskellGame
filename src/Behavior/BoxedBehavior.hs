module Behavior.BoxedBehavior 
    ( boundToDimension
    ) where


import Component.Position
import Component.Physics

import SFML.System.Vector2 (Vec2u (..), Vec2f (..))

boundToDimension :: (Position a, Physics a) => a -> Vec2u -> a
boundToDimension obj (Vec2u width height) = let 
    (Vec2f x y) = getPosition obj
    (Vec2f vx vy) = getVelocity obj
    newVx = vx * if x > fromIntegral width || x < 0 then (-1) else 1
    newVy = vy * if y > fromIntegral height || y < 0 then (-1) else 1
    in 
        setVelocity obj (Vec2f newVx newVy)