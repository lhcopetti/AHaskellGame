module PrefabObjects.AnimatedBlueBird
    ( createAnimatedBlueBird
    ) where

import SFML.System.Vector2

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createGameObject)
import GameObject.GameObjectTypes (GameObjectCreation, Ratio (..))
import Component.Draw.Animation.AnimationDrawing (createAnimation)
import Component.Draw.Animation.SpriteSheet (SpriteSheet (..), loadSpriteSheet, setScaleSpriteSheet)
import Component.Behavior.Behaviors (encloseByWrapAroundB)

import Paths_AHaskellGame

createAnimatedBlueBird :: Vec2f -> Vec2f -> GameObjectCreation
createAnimatedBlueBird pos vel = do
    liftIO $ putStrLn "Creating an animated blue bird"

    spriteSheetName <- liftIO $ getDataFileName "resources/sprites/blue-bird/FlyingGameCharacter_gimp.png"
    ss <- loadSpriteSheet spriteSheetName (Ratio 4 2)
    liftIO $ setScaleSpriteSheet ss (Vec2f 0.1 0.1)
    liftIO $ putStrLn $ "The number of sprites is: " ++ (show . length . sprites $ ss)

    let drw = createAnimation ss 25 id [4..7]

    let go = (createGameObject drw encloseByWrapAroundB pos vel)
    return go