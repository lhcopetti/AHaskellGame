module Behavior.BoxedBehavior 
    ( boundToDimension
    , wrapAround
    ) where


import Component.Position
import Component.Physics
import Container.TupleHelper (mapTuple)
import Vec2.Vec2Math (v2fToTuple)

import SFML.System.Vector2 (Vec2u (..), Vec2f (..))

boundToDimension :: (Position a, Physics a) => a -> Vec2u -> a
boundToDimension obj (Vec2u width height) = let 
    (Vec2f x y) = getPosition obj
    (Vec2f vx vy) = getVelocity obj
    newVx = vx * if x > fromIntegral width || x < 0 then (-1) else 1
    newVy = vy * if y > fromIntegral height || y < 0 then (-1) else 1
    in 
        setVelocity obj (Vec2f newVx newVy)

wrapAround :: (Position a, Physics a) => a -> Vec2u -> a
wrapAround obj (Vec2u width height) = setPosition obj (Vec2f newX newY)
        where
            (x, y) = mapTuple round . v2fToTuple . getPosition $ obj
            (signalX, signalY) = mapTuple signum (x, y)
            newX = fromIntegral (x `mod` (signalX * width))
            newY = fromIntegral (y `mod` (signalY * height))