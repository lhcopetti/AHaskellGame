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
stepPhysics deltaTime physicsWorld objs = stepWorld deltaTime physicsWorld >>
    mapM updatePhysics objs

stepGameObjects :: GameEnvironment -> [GameObject GoVoidState] -> GoVoidState -> IO ([GameObject GoVoidState], [GameObject GoVoidState], GoVoidState)
stepGameObjects env objs state = do
    let (newObjs, newState) = runMStack env state objs
    childrenObj <- getAllChildren newObjs
    newObjs' <- removeAllDead newObjs
    return (map removeChildren newObjs', childrenObj, newState)


getAllChildren :: [GameObject GoVoidState] -> IO [GameObject GoVoidState]
getAllChildren objs = do
    let childrenCreation = concatMap getChildren objs
    createdChildren <- createObjects childrenCreation
    return $ fromMaybe [] createdChildren

createObjects :: [GameObjectCreation] -> IO (Maybe [GameObject GoVoidState])
createObjects action = do
    newObjs <- forM action runMaybeT
    return (sequence newObjs)

removeAllDead :: [GameObject st] -> IO [GameObject st]
removeAllDead objs = do 
    let (alive, dead) = partition isAlive objs
    forM_ dead free
    return alive