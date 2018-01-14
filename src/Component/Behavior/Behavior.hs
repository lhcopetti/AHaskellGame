{-# LANGUAGE RankNTypes #-}
module Component.Behavior.Behavior
    ( BehaviorType
    , Behavior (..)
    ) where

import Control.Monad.Reader (Reader)

import Component.Position
import Component.Physics
import Killable
import GameEnv

type BehaviorType = forall a. (Position a, Physics a, Killable a) => a -> Reader GameEnvironment a

data Behavior = Behavior {  behave :: BehaviorType
                         }