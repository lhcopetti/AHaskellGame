module Updatable
    ( Updatable
    , update
    , UpdateType
    ) where

import GameEnv (GameEnvironment)
import Control.Monad.Trans.Reader (ReaderT)

type UpdateType a = a -> ReaderT GameEnvironment IO a

class Updatable a where 
    update :: UpdateType a