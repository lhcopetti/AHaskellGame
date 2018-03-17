{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Updatable
    ( Updatable
    , update
    , UpdateType
    , UpdateMStack
    , runMStack
    ) where

import GameEnv (GameEnvironment)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Trans.State (StateT, runStateT)

type UpdateMStack obj st = StateT st (Reader GameEnvironment) obj
type UpdateType obj st = obj -> UpdateMStack obj st

class Updatable obj st where
    update :: UpdateType obj st

runMStack :: Updatable obj st => GameEnvironment -> st -> [obj] -> ([obj], st)
runMStack env state = (`runReader` env) . (`runStateT` state) . mapM update