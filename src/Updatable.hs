module Updatable
    ( Updatable
    , update
    , UpdateType
    , SceneState (..)
    , UpdateMStack
    , runMStack
    ) where

import GameEnv (GameEnvironment)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Trans.State (StateT, runStateT)
import Conway

data SceneState = SceneState ConwayWorld
type UpdateMStack a = StateT SceneState (Reader GameEnvironment) a
type UpdateType a = a -> UpdateMStack a

class Updatable a where 
    update :: UpdateType a

runMStack :: Updatable a => GameEnvironment -> SceneState -> [a] -> ([a], SceneState)
runMStack env state = (`runReader` env) . (`runStateT` state) . mapM update