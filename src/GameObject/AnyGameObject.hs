{-# LANGUAGE ExistentialQuantification #-}

module GameObject.AnyGameObject
    ( AnyGameObject (..)
    , removeDeadAnyGameObjects
    , getChildrenAnyGameObjects
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
import Component.Physics.Physics ()

data AnyGameObject = forall a. ( Updatable a
                               , Drawable a
                               , Synchronizable a
                               , Killable a
                               , ChildBearer a
                               , PhysicsClass a)
                               => AGO a

instance Updatable AnyGameObject where
    update (AGO go) = liftM AGO (update go)

instance Synchronizable AnyGameObject where
    synchronize (AGO go) = synchronize go

instance Drawable AnyGameObject where
    draw window (AGO go) = draw window go

instance Killable AnyGameObject where
    isAlive         (AGO go) = isAlive go
    die             (AGO go) = AGO (die go)
    destroyResource (AGO go) = destroyResource go

instance ChildBearer AnyGameObject where
    getChildren     (AGO go) = getChildren go
    removeChildren  (AGO go) = AGO (removeChildren go)
    addChild child  (AGO go) = AGO (addChild child go)

getChildrenAnyGameObjects :: [AnyGameObject] -> IO [AnyGameObject]
getChildrenAnyGameObjects objs = do
    let childrenCreation = concatMap getChildren objs
    createdChildren <- createObjects childrenCreation
    return $ maybe [] (map AGO) createdChildren

createObjects :: [GameObjectCreation] -> IO (Maybe [GameObject])
createObjects action = do
    newObjs <- forM action runMaybeT
    return (sequence newObjs)

removeDeadAnyGameObjects :: [AnyGameObject] -> IO [AnyGameObject]
removeDeadAnyGameObjects objs = do 
    let (alive, dead) = partition isAlive objs
    forM_ dead destroyResource
    return alive

updatePhysicsAnyGameObjects :: [AnyGameObject] -> IO [AnyGameObject]
updatePhysicsAnyGameObjects = (`forM` updateAGO)
    where
        updateAGO (AGO a) = liftM AGO (updatePhysics a)