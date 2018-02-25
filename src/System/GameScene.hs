module System.GameScene
    ( GameScene (..)
    , updateGameScene
    ) where

import Control.Monad (forM_)

import System.GameStepper (stepPhysics, stepGameObjects)
import GameObject.AnyGameObject (AnyGameObject)
import Physics.PhysicsTypes (PhysicsWorld)
import Drawable
import Synchronizable
import GameEnv (GameEnvironment (..))

data GameScene = GameScene  { physicsWorld :: PhysicsWorld
                            , gameObjects  :: [AnyGameObject]
                            }


instance Drawable GameScene where
    draw wnd scene = forM_ (gameObjects scene) (draw wnd)

instance Synchronizable GameScene where
    synchronize scene = forM_ (gameObjects scene) synchronize


updateGameScene :: GameScene -> GameEnvironment -> IO (GameScene, [AnyGameObject])
updateGameScene (GameScene physicsWorld objs) env = do
    objs' <- stepPhysics (1 / 60) physicsWorld objs
    (newObjs, childrenObj) <- stepGameObjects env objs'
    return (GameScene physicsWorld newObjs, childrenObj)