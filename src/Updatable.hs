module Updatable
    ( Updatable
    , update
    , UpdateType
    , StateType
    , UpdateMStack
    ) where

import GameEnv (GameEnvironment)
import Control.Monad.Reader (Reader)
import Control.Monad.Trans.State (StateT)

type StateType = Integer
type UpdateMStack a = StateT StateType (Reader GameEnvironment) a
type UpdateType a = a -> UpdateMStack a

class Updatable a where 
    update :: UpdateType a