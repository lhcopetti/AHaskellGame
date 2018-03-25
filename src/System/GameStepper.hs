module System.GameStepper
    ( stepPhysics
    , stepGameObjects
    ) where

import Control.Monad (forM, forM_)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Maybe (runMaybeT)

import GameEnv (GameEnvironment (..))
import GameObject.GameObject ()
import GameObject.GameObjectTypes
import Physics.PhysicsWorld (stepWorld)
import Physics.PhysicsTypes (PhysicsWorld)
import Updatable
import Killable
import ChildBearer
import NativeResource
import Component.Physics.PhysicsClass
import Component.Physics.Physics ()


stepPhysics :: Float -> PhysicsWorld -> [GameObject st] -> IO [GameObject st]
stepPhysics deltaTime physicsWorld objs = do
    stepWorld deltaTime physicsWorld
    mapM updatePhysics objs

stepGameObjects :: GameEnvironment -> [GameObject st] -> st -> IO ([GameObject st], [GameObject st], st)
stepGameObjects env objs state = do
    let (newObjs, newState) = runMStack env state objs
    childrenObj <- getAllChildren newObjs
    newObjs' <- removeAllDead newObjs
    return (map removeChildren newObjs', childrenObj, newState)


getAllChildren :: [GameObject st] -> IO [GameObject st]
getAllChildren objs = do
    let childrenCreation = concatMap getChildren objs
    createdChildren <- createObjects childrenCreation
    return $ fromMaybe [] createdChildren

createObjects :: [ChildGameObjectCreation st] -> IO (Maybe [GameObject st])
createObjects action = do
    newObjs <- forM action runGameObjectCreation
    return (sequence newObjs)

runGameObjectCreation :: ChildGameObjectCreation st -> IO (Maybe (GameObject st))
runGameObjectCreation (CGOC action) = runMaybeT action


removeAllDead :: [GameObject st] -> IO [GameObject st]
removeAllDead objs = do 
    let (alive, dead) = partition isAlive objs
    forM_ dead free
    return alive