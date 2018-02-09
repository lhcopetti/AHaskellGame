{-# LANGUAGE ExistentialQuantification #-}

module GameObject.AnyGameObject
    ( AnyGameObject (..)
    , updateAnyGameObject
    , drawAnyGameObject
    , synchronizeGameObject
    , isAliveAnyGameObject
    , removeDeadAnyGameObjects
    , getChildrenAnyGameObjects
    , removeChildrenAnyGameObject
    , updatePhysicsAnyGameObjects
    ) where

import Control.Monad (forM_, forM, liftM)
import Data.List (partition)
import Control.Monad.Trans.Maybe (runMaybeT)

import GameObject.GameObject ()
import GameObject.GameObjectTypes (GameObjectCreation, GameObject)
import Drawable
import Updatable
import Synchronizable
import Killable
import ChildBearer
import Component.Physics.PhysicsClass

data AnyGameObject = forall a. ( Updatable a
                               , Drawable a
                               , Synchronizable a
                               , Killable a
                               , ChildBearer a
                               , PhysicsClass a)
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

getChildrenAnyGameObject :: AnyGameObject -> [GameObjectCreation]
getChildrenAnyGameObject (AGO a) = getChildren a

removeChildrenAnyGameObject :: AnyGameObject -> AnyGameObject
removeChildrenAnyGameObject (AGO a) = AGO $ removeChildren a

getChildrenAnyGameObjects :: [AnyGameObject] -> IO [AnyGameObject]
getChildrenAnyGameObjects objs = do
    let childrenCreation = concatMap getChildrenAnyGameObject objs
    createdChildren <- createObjects childrenCreation
    return $ maybe [] (map AGO) createdChildren

createObjects :: [GameObjectCreation] -> IO (Maybe [GameObject])
createObjects action = do
    newObjs <- forM action runMaybeT
    return (sequence newObjs)

removeDeadAnyGameObjects :: [AnyGameObject] -> IO [AnyGameObject]
removeDeadAnyGameObjects objs = do 
    let (alive, dead) = partition isAliveAnyGameObject objs
    forM_ dead (\(AGO a) -> destroyResource a)
    return alive

updatePhysicsAnyGameObjects :: [AnyGameObject] -> IO [AnyGameObject]
updatePhysicsAnyGameObjects = (`forM` updateAGO)
    where
        updateAGO (AGO a) = liftM AGO (updatePhysics a)