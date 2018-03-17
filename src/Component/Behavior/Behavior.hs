{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Component.Behavior.Behavior
    ( BehaviorType
    , Behavior (..)
    , Behavioral
    , setBehavior
    , setBehaviorT
    ) where

import GameObject.GameObjectTypes

class Behavioral obj st | obj -> st where
    setBehavior :: Behavior st -> obj -> obj
    setBehaviorT :: BehaviorType st -> obj -> obj