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

type BehaviorType = forall a.   ( Position a
                                , PhysicsClass a
                                , Killable a
                                , DrawingInbox a
                                , Behavioral a
                                ) => a -> Reader GameEnvironment a

data Behavior = Behavior {  behave :: BehaviorType
                         }

class Behavioral a where
    setBehavior :: Behavior -> a -> a
    setBehaviorT :: BehaviorType -> a -> a