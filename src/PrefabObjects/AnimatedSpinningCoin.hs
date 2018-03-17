module PrefabObjects.AnimatedSpinningCoin
    ( createSpinningCoin
    ) where

import SFML.System.Vector2

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createSimplePhysicsGO)
import GameObject.GameObjectTypes (GameObjectCreation, Ratio (..))
import Component.Draw.Animation.AnimationDrawing (createAnimation)
import Component.Draw.Animation.SpriteSheet (SpriteSheet (..), loadSpriteSheet)
import Component.Behavior.Behaviors (encloseByWrapAroundB)

import Paths_AHaskellGame

createSpinningCoin :: Vec2f -> Vec2f -> GameObjectCreation st
createSpinningCoin pos vel = do
    liftIO $ putStrLn "Creating an animated spinning coin"
    
    spriteSheetName <- liftIO $ getDataFileName "resources/sprites/spinning-coin/coin_altered.png"
    ss <- loadSpriteSheet spriteSheetName (Ratio 6 1)
    liftIO $ putStrLn $ "The number of sprites is: " ++ (show . length . sprites $ ss)

    let animated = createAnimation ss 15 id [0..6]
    -- let drw = createNewDrawing animated
    
    let go = (createSimplePhysicsGO animated encloseByWrapAroundB pos vel)
    return go