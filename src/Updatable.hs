{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Updatable
    ( InternalSceneState (..)
    , SceneState (..)
    , Updatable
    , update
    , UpdateType
    , UpdateMStack
    , runMStack
    ) where

import GameEnv (GameEnvironment)
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Trans.State (StateT, runStateT)

data InternalSceneState = InternalSceneState { paused :: Bool
                                             }

data SceneState st = SceneState { sceneState :: InternalSceneState
                                , userState :: st
                                }

type UpdateMStack obj st = StateT (SceneState st) (Reader GameEnvironment) obj
type UpdateType obj st = obj -> UpdateMStack obj st

class Updatable obj st where
    update :: UpdateType obj st

runMStack :: Updatable obj st => GameEnvironment -> SceneState st -> [obj] -> ([obj], SceneState st)
runMStack env state = (`runReader` env) . (`runStateT` state) . mapM update
