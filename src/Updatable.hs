module Updatable
    where

import GameEnv (GameEnvironment)
import Control.Monad.Trans.Reader (ReaderT)

class Updatable a where 
    update :: a -> ReaderT GameEnvironment IO a