{-# LANGUAGE RankNTypes #-}
module Component.Behavior.Behavior
    ( BehaviorType
    , Behavior (..)
    , Behavioral
    , setBehavior
    , setBehaviorT
    ) where

import GameObject.GameObjectTypes

class Behavioral a where
    setBehavior :: Behavior -> a -> a
    setBehaviorT :: BehaviorType -> a -> a