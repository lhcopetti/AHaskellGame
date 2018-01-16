module Random.RandomState
    ( randomState
    , randomRState
    ) where

import System.Random (random, randomR, StdGen)
import Control.Monad.Trans.State (State, state, StateT)
import Control.Monad.Trans.Maybe (MaybeT)

randomState :: State StdGen Integer
randomState = state random

randomRState :: (Float, Float) -> StateT StdGen (MaybeT IO) Float
randomRState = state . randomR
        