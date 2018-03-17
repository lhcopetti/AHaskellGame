{-# LANGUAGE RankNTypes #-}
module Component.Behavior.RotationalBehavior
    ( rotate
    ) where

import Component.Behavior.Behavior
import GameObject.GameObject ()
import Component.Position

rotate :: Float -> BehaviorType st
rotate degree obj = let
    currentDegree = getRotation obj
    newDegree = currentDegree + degree 
    in 
        return (setRotation newDegree obj)