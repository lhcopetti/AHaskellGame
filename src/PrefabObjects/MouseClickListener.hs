module PrefabObjects.MouseClickListener
    ( mkMouseClickListener
    ) where

import SFML.System.Vector2
import SFML.Graphics.Color (white, blue)

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createSimplePhysicsGO)
import GameObject.GameObjectTypes
import System.Messaging.Handler.RunMessageHandler (runMessageT)
import System.Messaging.Handler.PushMessageHandler (pushMessage)
import System.Messaging.Messages.TransformableMessage (setOriginMsg)
import System.Messaging.Messages.ShapeMessage (setFillColorMsg)
import Component.Input.Input (isLeftMousePressed)
import Component.Draw.Drawing ()
import Component.Draw.CircleDrawing (createCircle)
import Component.Draw.CompositeDrawing (createComposite)
import Component.Draw.TextDrawing (createText)
import Vec2.Vec2Math (zero)

mkMouseClickListener :: GameObjectCreation st
mkMouseClickListener = do
    let newBall = createCircle 5 white
    liftIO $ putStrLn "Creating a mouse click listener"
    lCircle <- newBall
    mCircle <- newBall
    rCircle <- newBall
    text <- createText 15 "Mouse clicks"
    liftIO $ runMessageT (setOriginMsg (Vec2f 30 20)) text
    liftIO $ runMessageT (setOriginMsg (Vec2f (-10) 0)) lCircle
    liftIO $ runMessageT (setOriginMsg (Vec2f 10 0)) rCircle
    drw <- createComposite [lCircle, mCircle, rCircle, text]
    let beh = Behavior changeColorOnMousePress
    let go = createSimplePhysicsGO drw beh (Vec2f 0 0) zero
    return go

changeColorOnMousePress :: BehaviorType st
changeColorOnMousePress obj = do
    pressed <- isLeftMousePressed
    let color = if pressed then blue else white
        msg = setFillColorMsg color
    pushMessage msg obj