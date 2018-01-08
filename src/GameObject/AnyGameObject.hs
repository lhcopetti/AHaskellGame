{-# LANGUAGE ExistentialQuantification #-}

module GameObject.AnyGameObject
    ( AnyGameObject (..)
    , updateAnyGameObject
    , drawAnyGameObject
    ) where

import GameEnv (GameEnvironment)
import Drawable
import Updatable

data AnyGameObject = forall a. (Updatable a, Drawable a) => AGO a

updateAnyGameObject :: UpdateType AnyGameObject
updateAnyGameObject (AGO obj) = do 
    newObj <- update obj
    return (AGO newObj)

drawAnyGameObject :: DrawType AnyGameObject
drawAnyGameObject window (AGO obj) = draw window obj