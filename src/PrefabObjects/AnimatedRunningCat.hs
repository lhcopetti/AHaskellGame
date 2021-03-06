module PrefabObjects.AnimatedRunningCat
    ( createAnimatedRunningCat
    ) where


import SFML.System.Vector2

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createSimplePhysicsGO)
import GameObject.GameObjectTypes (GameObjectCreation, Ratio (..))
import Component.Draw.Animation.AnimationDrawing (createAnimation)
import Component.Draw.Animation.SpriteSheet (SpriteSheet (..), loadSpriteSheet)
import Component.Behavior.Behaviors (encloseByWrapAroundB)

createAnimatedRunningCat :: Vec2f -> Vec2f -> GameObjectCreation st
createAnimatedRunningCat pos vel = do
    liftIO $ putStrLn "Creating an animated running cat"

    ss <- loadSpriteSheet "running-cat/runningcat-20.png" (Ratio 2 4)
    liftIO $ putStrLn $ "The number of sprites is: " ++ (show . length . sprites $ ss)

    let drw = createAnimation ss 15 id [0..7]
    
    let go = (createSimplePhysicsGO drw encloseByWrapAroundB pos vel)
    return go