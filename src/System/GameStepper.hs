module System.GameStepper
    ( stepPhysics
    , stepGameObjects
    ) where

import Control.Monad (forM, forM_)
import Data.List (partition)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.State

import Control.Monad.Reader (runReader)

import GameEnv (GameEnvironment (..))
import GameObject.AnyGameObject (AnyGameObject (..))
import GameObject.GameObject ()
import GameObject.GameObjectTypes (GameObjectCreation, GameObject)
import Physics.PhysicsWorld (stepWorld)
import Physics.PhysicsTypes (PhysicsWorld)
import Updatable
import Killable
import ChildBearer
import NativeResource
import Component.Physics.PhysicsClass
import Component.Physics.Physics ()


stepPhysics :: Float -> PhysicsWorld -> [AnyGameObject] -> IO [AnyGameObject]
stepPhysics deltaTime physicsWorld objs = stepWorld deltaTime physicsWorld >>
    mapM updatePhysics objs

stepGameObjects :: GameEnvironment -> [AnyGameObject] -> StateType -> IO ([AnyGameObject], [AnyGameObject], StateType)
stepGameObjects env objs state = do
    let (newObjs, newState) = runMStack env state objs
    childrenObj <- getChildrenAnyGameObjects newObjs
    newObjs' <- removeDeadAnyGameObjects newObjs
    return (map removeChildren newObjs', childrenObj, newState)


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
    forM_ dead free
    return alive