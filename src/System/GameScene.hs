{-# LANGUAGE RecordWildCards #-}
module System.GameScene
    ( GameScene (..)
    , updateGameScene
    ) where

import Control.Monad (forM_)

import System.GameStepper (stepPhysics, stepGameObjects)
import Physics.PhysicsTypes (PhysicsWorld)
import Physics.PhysicsWorld ()
import Drawable
import Synchronizable
import NativeResource
import Component.Draw.ZOrderable
import Updatable (SceneState)
import GameEnv (GameEnvironment (..))
import GameObject.GameObjectTypes

data GameScene = GameScene  { physicsWorld :: PhysicsWorld
                            , gameObjects  :: [GameObject]
                            , gameState    :: SceneState
                            }


instance Drawable GameScene where
    draw wnd scene = 
        let objs = gameObjects scene
            in forM_ (orderByZ objs) (draw wnd)

instance Synchronizable GameScene where
    synchronize scene = forM_ (gameObjects scene) synchronize

instance NativeResource GameScene where
    free GameScene {..} = mapM_ free gameObjects >> free physicsWorld


updateGameScene :: GameScene -> GameEnvironment -> IO (GameScene, [GameObject])
updateGameScene GameScene {..} env = do
    objs' <- stepPhysics (1 / 60) physicsWorld gameObjects
    (newObjs, childrenObj, newState) <- stepGameObjects env objs' gameState
    return (GameScene physicsWorld newObjs newState, childrenObj)