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
import GameObject.GameObjectTypes (GameObjectCreation, GameObject)
import Physics.PhysicsWorld (stepWorld)
import Physics.PhysicsTypes (PhysicsWorld)
import Updatable
import Killable
import ChildBearer
import NativeResource
import Component.Physics.PhysicsClass
import Component.Physics.Physics ()


stepPhysics :: Float -> PhysicsWorld -> [GameObject] -> IO [GameObject]
stepPhysics deltaTime physicsWorld objs = stepWorld deltaTime physicsWorld >>
    mapM updatePhysics objs

stepGameObjects :: GameEnvironment -> [GameObject] -> SceneState -> IO ([GameObject], [GameObject], SceneState)
stepGameObjects env objs state = do
    let (newObjs, newState) = runMStack env state objs
    childrenObj <- getAllChildren newObjs
    newObjs' <- removeAllDead newObjs
    return (map removeChildren newObjs', childrenObj, newState)


getAllChildren :: [GameObject] -> IO [GameObject]
getAllChildren objs = do
    let childrenCreation = concatMap getChildren objs
    createdChildren <- createObjects childrenCreation
    return $ fromMaybe [] createdChildren

createObjects :: [GameObjectCreation] -> IO (Maybe [GameObject])
createObjects action = do
    newObjs <- forM action runMaybeT
    return (sequence newObjs)

removeAllDead :: [GameObject] -> IO [GameObject]
removeAllDead objs = do 
    let (alive, dead) = partition isAlive objs
    forM_ dead free
    return alive