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

createSpinningCoin :: Vec2f -> Vec2f -> GameObjectCreation st
createSpinningCoin pos vel = do
    liftIO $ putStrLn "Creating an animated spinning coin"

    ss <- loadSpriteSheet "spinning-coin/coin_altered.png" (Ratio 6 1)
    liftIO $ putStrLn $ "The number of sprites is: " ++ (show . length . sprites $ ss)

    let animated = createAnimation ss 15 id [0..6]
    let go = (createSimplePhysicsGO animated encloseByWrapAroundB pos vel)
    return go