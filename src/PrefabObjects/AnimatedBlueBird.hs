module PrefabObjects.AnimatedBlueBird
    ( createAnimatedBlueBird
    ) where

import SFML.System.Vector2

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createGameObject)
import GameObject.GameObjectTypes (GameObjectCreation, GameObject (..))
import Component.Animation.Animation (createNewDrawing, createAnimation)
import Component.Animation.SpriteSheet (SpriteSheet (..), loadSpriteSheet)
import Component.Behavior.Behaviors (encloseByWrapAroundB)

import Paths_AHaskellGame

createAnimatedBlueBird :: Vec2f -> Vec2f -> GameObjectCreation
createAnimatedBlueBird pos vel = do
    liftIO $ putStrLn "Creating an animated blue bird"
    
    spriteSheetName <- liftIO $ getDataFileName "resources/sprites/blue-bird/blue-bird-10%-resized.png"
    ss <- loadSpriteSheet spriteSheetName (4, 2)
    liftIO $ putStrLn $ "The number of sprites is: " ++ (show . length . sprites $ ss)

    let animated = createAnimation ss 15 id
    
    let drw = createNewDrawing animated
    
    let go = (createGameObject drw encloseByWrapAroundB pos vel)
    return go { animationComp = Just animated }