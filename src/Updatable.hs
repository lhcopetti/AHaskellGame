module Updatable
    ( Updatable
    , update
    , UpdateType
    ) where

import GameEnv (GameEnvironment)
import Control.Monad.Reader (Reader)
import Control.Monad.Trans.State (StateT)

type UpdateType a = a -> StateT Int (Reader GameEnvironment) a

class Updatable a where 
    update :: UpdateType a