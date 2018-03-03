module Updatable
    ( Updatable
    , update
    , UpdateType
    , StateType
    , UpdateMStack
    , runMStack
    ) where

import GameEnv (GameEnvironment)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Trans.State (StateT, runStateT)
import Conway

type StateType = ConwayWorld
type UpdateMStack a = StateT StateType (Reader GameEnvironment) a
type UpdateType a = a -> UpdateMStack a

class Updatable a where 
    update :: UpdateType a

runMStack :: Updatable a => GameEnvironment -> StateType -> [a] -> ([a], StateType)
runMStack env state = (`runReader` env) . (`runStateT` state) . mapM update