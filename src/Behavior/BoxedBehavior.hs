module Behavior.BoxedBehavior 
    ( boundToDimension
    , wrapAround
    , wrapAroundValue
    ) where

import Control.Applicative (ZipList (..))

import Component.Position
import Component.Physics.PhysicsClass

import SFML.System.Vector2 (Vec2u (..), Vec2f (..))

boundToDimension :: (Position a, PhysicsClass a) => a -> Vec2u -> a
boundToDimension obj (Vec2u width height) = let 
    (Vec2f x y) = getPosition obj
    (Vec2f vx vy) = getVelocity obj
    newVx = vx * if x > fromIntegral width || x < 0 then (-1) else 1
    newVy = vy * if y > fromIntegral height || y < 0 then (-1) else 1
    in 
        setVelocity obj (Vec2f newVx newVy)

wrapAround :: (Position a) => a -> Vec2u -> a
wrapAround obj dimension = setPosition obj newPos 
    where 
        pos = getPosition obj
        newPos = wrapAroundPos pos dimension

wrapAroundPos :: Vec2f -> Vec2u -> Vec2f 
wrapAroundPos (Vec2f px py) (Vec2u width height) = let
            position = ZipList [px, py]
            dimension = ZipList [width, height]
            ZipList [nx, ny] = wrapAroundFloat <$> position <*> dimension
            in 
                Vec2f nx ny

wrapAroundFloat :: Float -> Word -> Float
wrapAroundFloat value maxValue = let 
    iValue = round value
    iMax = round . fromIntegral $ maxValue
    in 
        fromIntegral (wrapAroundValue iValue iMax)

wrapAroundValue :: Integral a => a -> a -> a
wrapAroundValue value max = value `mod` max