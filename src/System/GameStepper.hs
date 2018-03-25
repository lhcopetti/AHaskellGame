{-# LANGUAGE RecordWildCards #-}
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

stepGameObjects :: GameEnvironment -> GameScene st -> IO ([GameObject st], [GameObject st], st)
stepGameObjects env GameScene {..} = do
    let (newObjs, newState) = runMStack env gameState gameObjects
    childrenObj <- createAllChildren physicsWorld newObjs
    newObjs' <- removeAllDead newObjs
    return (map removeChildren newObjs', childrenObj, newState)


createAllChildren :: PhysicsWorld -> [GameObject st] -> IO [GameObject st]
createAllChildren phyWorld objs = do
    let childrenCreation = concatMap getChildren objs
    createdChildren <- createObjects phyWorld childrenCreation
    return $ fromMaybe [] createdChildren

createObjects :: PhysicsWorld -> [ChildGameObjectCreation st] -> IO (Maybe [GameObject st])
createObjects phyWorld action = do
    newObjs <- forM action (runGameObjectCreation phyWorld)
    return (sequence newObjs)

runGameObjectCreation :: PhysicsWorld -> ChildGameObjectCreation st -> IO (Maybe (GameObject st))
runGameObjectCreation _ (CGOC action) = runMaybeT action
runGameObjectCreation phyWorld (PGOC action) = runMaybeT (action phyWorld)


removeAllDead :: [GameObject st] -> IO [GameObject st]
removeAllDead objs = do 
    let (alive, dead) = partition isAlive objs
    forM_ dead free
    return alive