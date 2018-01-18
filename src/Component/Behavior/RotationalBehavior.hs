{-# LANGUAGE RankNTypes #-}
module Component.Behavior.RotationalBehavior
    ( rotate
    ) where

import SFML.System.Vector2 (Vec2f)

import Component.Behavior.Behavior
import Component.Position

rotate :: Float -> BehaviorType
rotate degree obj = let
    currentDegree = getRotation obj
    newDegree = currentDegree + degree 
    in 
        return (setRotation newDegree obj)