{-# LANGUAGE RecordWildCards #-}
module System.GameScene
    ( GameScene (..)
    , updateGameScene
    ) where

import Control.Monad (forM_)

import System.GameStepper (stepPhysics, stepGameObjects)
import GameObject.AnyGameObject (AnyGameObject)
import Physics.PhysicsTypes (PhysicsWorld)
import Physics.PhysicsWorld ()
import Drawable
import Synchronizable
import NativeResource
import GameEnv (GameEnvironment (..))

data GameScene = GameScene  { physicsWorld :: PhysicsWorld
                            , gameObjects  :: [AnyGameObject]
                            , gameState    :: Int
                            }


instance Drawable GameScene where
    draw wnd scene = forM_ (gameObjects scene) (draw wnd)

instance Synchronizable GameScene where
    synchronize scene = forM_ (gameObjects scene) synchronize

instance NativeResource GameScene where
    free GameScene {..} = mapM_ free gameObjects >> free physicsWorld


updateGameScene :: GameScene -> GameEnvironment -> IO (GameScene, [AnyGameObject])
updateGameScene GameScene {..} env = do
    objs' <- stepPhysics (1 / 60) physicsWorld gameObjects
    (newObjs, childrenObj, newState) <- stepGameObjects env objs' gameState
    return (GameScene physicsWorld newObjs newState, childrenObj)