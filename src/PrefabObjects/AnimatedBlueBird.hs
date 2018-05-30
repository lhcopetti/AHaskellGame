module PrefabObjects.AnimatedBlueBird
    ( createAnimatedBlueBird
    ) where

import SFML.System.Vector2

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createSimplePhysicsGO)
import GameObject.GameObjectTypes (GameObjectCreation, Ratio (..))
import Component.Draw.Animation.AnimationDrawing (createAnimation)
import Component.Draw.Animation.SpriteSheet (SpriteSheet (..), loadSpriteSheet, setScaleSpriteSheet)
import Component.Behavior.Behaviors (encloseByWrapAroundB)

createAnimatedBlueBird :: Vec2f -> Vec2f -> GameObjectCreation st
createAnimatedBlueBird pos vel = do
    liftIO $ putStrLn "Creating an animated blue bird"

    ss <- loadSpriteSheet "blue-bird/FlyingGameCharacter_gimp.png" (Ratio 4 2)
    liftIO $ setScaleSpriteSheet ss (Vec2f 0.1 0.1)
    liftIO $ putStrLn $ "The number of sprites is: " ++ (show . length . sprites $ ss)

    let drw = createAnimation ss 25 id [4..7]

    let go = (createSimplePhysicsGO drw encloseByWrapAroundB pos vel)
    return go