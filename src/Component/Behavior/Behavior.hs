{-# LANGUAGE RankNTypes #-}
module Component.Behavior.Behavior
    ( BehaviorType
    , Behavior (..)
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
                                ) => a -> Reader GameEnvironment a

data Behavior = Behavior {  behave :: BehaviorType
                         }