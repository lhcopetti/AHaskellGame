{-# LANGUAGE RankNTypes #-}
module Component.Behavior.Behavior
    ( BehaviorType
    , Behavior (..)
    ) where

import Control.Monad.Reader (Reader)

import Component.Position
import Component.Physics
import GameEnv

type BehaviorType = forall a. (Position a, Physics a) => a -> Reader GameEnvironment a

data Behavior = Behavior {  behave :: BehaviorType
                         }