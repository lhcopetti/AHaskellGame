{-# LANGUAGE RecordWildCards #-}
module System.GameScene
    ( GameScene (..)
    , updateGameScene
    ) where

import Control.Monad (forM_)

import System.GameStepper (stepPhysics, stepGameObjects)
import Physics.PhysicsTypes (PhysicsWorld)
import Physics.PhysicsWorld (getCollisionData)
import Drawable
import Synchronizable
import NativeResource
import Component.Draw.ZOrderable
import GameEnv (GameEnvironment (..), GameTime (..), updateCollisionData)
import GameObject.GameObjectTypes

data GameScene a = GameScene    { physicsWorld :: PhysicsWorld
                                , gameObjects  :: [GameObject a]
                                , gameState    :: a
                                }


instance Drawable (GameScene a) where
    draw wnd scene = 
        let objs = gameObjects scene
            in forM_ (orderByZ objs) (draw wnd)

instance Synchronizable (GameScene a) where
    synchronize scene = forM_ (gameObjects scene) synchronize

instance NativeResource (GameScene a) where
    free GameScene {..} = mapM_ free gameObjects >> free physicsWorld


updateGameScene :: GameScene a -> GameEnvironment -> IO (GameScene a, [GameObject a])
updateGameScene GameScene {..} env = do
    objs' <- stepPhysics dt physicsWorld gameObjects
    collData <- getCollisionData physicsWorld
    let collEnv = updateCollisionData collData env
    (newObjs, childrenObj, newState) <- stepGameObjects collEnv objs' gameState
    return (GameScene physicsWorld newObjs newState, childrenObj)
        where
            dt = realToFrac . deltaTime . time $ env