module Updatable
    ( Updatable
    , update
    , UpdateType
    ) where

import GameEnv (GameEnvironment)
import Control.Monad.Reader (Reader)

type UpdateType a = a -> Reader GameEnvironment a

class Updatable a where 
    update :: UpdateType a