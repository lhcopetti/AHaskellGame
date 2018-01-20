{-# LANGUAGE RankNTypes #-}
module Component.Behavior.Behavior
    ( BehaviorType
    , Behavior (..)
    , Behavioral
    , setBehavior
    , setBehaviorT
    ) where

import Control.Monad.Reader (Reader)

import Component.Position
import Component.Physics.PhysicsClass
import System.Messaging.DrawingMessage (DrawingInbox)
import Killable
import GameEnv
import GameObject.GameObjectTypes

class Behavioral a where
    setBehavior :: Behavior -> a -> a
    setBehaviorT :: BehaviorType -> a -> a