module PrefabObjects.BallInputAware
    ( createBallInputAware
    ) where
    
import SFML.System.Vector2
import SFML.Graphics.Color (green)
import SFML.Window.Keyboard

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createGameObject)
import GameObject.GameObjectTypes
import Component.Draw.Drawing (setOriginDrawing)
import Component.Draw.CircleDrawing (createCenteredCircle)
import Component.Draw.CompositeDrawing (createComposite)
import Component.Draw.TextDrawing (createText)
import Component.Behavior.Behaviors (noopB)
import Vec2.Vec2Math (zero)

import Component.Input.Inputs (dieOnKeyPressing)

createBallInputAware :: Vec2f -> GameObjectCreation
createBallInputAware pos = do
    liftIO $ putStrLn "Creating a ball input aware"
    circle <- createCenteredCircle 10 green
    text <- createText 15 "Dies when 'q' is pressed"
    liftIO $ setOriginDrawing text (Vec2f 10 0)
    drw <- createComposite [circle, text]
    let input = Input (dieOnKeyPressing KeyQ)
    let go = createGameObject drw noopB pos zero 
    return (go { inputComp = input })