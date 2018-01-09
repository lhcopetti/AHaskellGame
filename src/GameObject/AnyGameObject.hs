{-# LANGUAGE ExistentialQuantification #-}

module GameObject.AnyGameObject
    ( AnyGameObject (..)
    , updateAnyGameObject
    , drawAnyGameObject
    , synchronizeGameObject
    ) where

import GameEnv (GameEnvironment)
import Drawable
import Updatable
import Synchronizable

data AnyGameObject = forall a. (Updatable a, Drawable a, Synchronizable a) => AGO a

updateAnyGameObject :: UpdateType AnyGameObject
updateAnyGameObject (AGO obj) = do 
    newObj <- update obj
    return (AGO newObj)

synchronizeGameObject :: SynchronizableType AnyGameObject
synchronizeGameObject (AGO obj) = synchronize obj

drawAnyGameObject :: DrawType AnyGameObject
drawAnyGameObject window (AGO obj) = draw window obj