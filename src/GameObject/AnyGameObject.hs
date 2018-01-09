{-# LANGUAGE ExistentialQuantification #-}

module GameObject.AnyGameObject
    ( AnyGameObject (..)
    , updateAnyGameObject
    , drawAnyGameObject
    , synchronizeGameObject
    , isAliveAnyGameObject
    , removeDeadAnyGameObjects
    ) where

import Control.Monad (forM_)
import Data.List (partition)

import GameEnv (GameEnvironment)
import Drawable
import Updatable
import Synchronizable
import Killable

data AnyGameObject = forall a. ( Updatable a
                               , Drawable a
                               , Synchronizable a
                               , Killable a) 
                               => AGO a

updateAnyGameObject :: UpdateType AnyGameObject
updateAnyGameObject (AGO obj) = do 
    newObj <- update obj
    return (AGO newObj)

synchronizeGameObject :: SynchronizableType AnyGameObject
synchronizeGameObject (AGO obj) = synchronize obj

drawAnyGameObject :: DrawType AnyGameObject
drawAnyGameObject window (AGO obj) = draw window obj

isAliveAnyGameObject :: AnyGameObject -> Bool
isAliveAnyGameObject (AGO a) = isAlive a

removeDeadAnyGameObjects :: [AnyGameObject] -> IO [AnyGameObject]
removeDeadAnyGameObjects objs = do 
    let (alive, dead) = partition isAliveAnyGameObject objs
    forM_ dead (\(AGO a) -> destroyResource a)
    return alive