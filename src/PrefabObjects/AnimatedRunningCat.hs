module PrefabObjects.AnimatedRunningCat
    ( createAnimatedRunningCat
    ) where


import SFML.System.Vector2

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createGameObject)
import GameObject.GameObjectTypes (GameObjectCreation, GameObject (..))
import Component.Animation.Animation (createNewDrawing, createAnimation)
import Component.Animation.SpriteSheet (SpriteSheet (..), loadSpriteSheet)
import Component.Behavior.Behaviors (encloseByWrapAroundB)

import Paths_AHaskellGame

createAnimatedRunningCat :: Vec2f -> Vec2f -> GameObjectCreation
createAnimatedRunningCat pos vel = do
    liftIO $ putStrLn "Creating an animated running cat"
    
    spriteSheetName <- liftIO $ getDataFileName "resources/sprites/running-cat/runningcat-20.png"
    ss <- loadSpriteSheet spriteSheetName (2, 4)
    liftIO $ putStrLn $ "The number of sprites is: " ++ (show . length . sprites $ ss)

    let animated = createAnimation ss 15 id [0..7]
    let drw = createNewDrawing animated
    
    let go = (createGameObject drw encloseByWrapAroundB pos vel)
    return go { animationComp = Just animated }