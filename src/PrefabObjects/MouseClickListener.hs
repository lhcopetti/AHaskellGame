module PrefabObjects.MouseClickListener
    ( mkMouseClickListener
    ) where

import SFML.System.Vector2
import SFML.Graphics.Color

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createSimplePhysicsGO)
import GameObject.GameObjectTypes
import System.Messaging.Handler.RunMessageHandler (runMessageT)
import System.Messaging.Handler.PushMessageHandler (pushNamedMessage)
import System.Messaging.Messages.TransformableMessage (setOriginMsg)
import System.Messaging.Messages.ShapeMessage (setFillColorMsg)
import qualified System.Input.MouseSnapshot as M (MButton (..))
import Component.Input.Input
import Component.Draw.Drawing ()
import Component.Draw.CircleDrawing (createCircle)
import Component.Draw.CompositeDrawing (createComposite)
import Component.Draw.TextDrawing (createText)
import Component.Draw.NamedDrawing (createNamedDrawingM)
import Component.Behavior.Behaviors (behaveAllB)
import Vec2.Vec2Math (zero)

mkMouseClickListener :: GameObjectCreation st
mkMouseClickListener = do
    let newBall name = createNamedDrawingM name $ createCircle 5 white
    liftIO $ putStrLn "Creating a mouse click listener"
    lCircle <- newBall "left"
    mCircle <- newBall "middle"
    rCircle <- newBall "right"
    text <- createText 15 "Mouse clicks"
    liftIO $ runMessageT (setOriginMsg (Vec2f 30 20)) text
    liftIO $ runMessageT (setOriginMsg (Vec2f 10 0)) lCircle
    liftIO $ runMessageT (setOriginMsg (Vec2f (-10) 0)) rCircle
    drw <- createComposite [lCircle, mCircle, rCircle, text]
    let behs = Behavior <$> [changeColorOnRightMousePress, changeColorOnLeftMousePress]
    let go = createSimplePhysicsGO drw (behaveAllB behs) (Vec2f 0 0) zero
    return go

changeColorOnLeftMousePress :: BehaviorType st
changeColorOnLeftMousePress obj = do
    pressed <- isMousePressed M.MLeft
    let color
            | pressed       = blue
            | otherwise     = white
        msg = setFillColorMsg color
    pushNamedMessage "left" msg obj

changeColorOnRightMousePress :: BehaviorType st
changeColorOnRightMousePress obj = do
    pressed <- isMousePressed M.MRight
    let color
            | pressed       = blue
            | otherwise     = white
        msg = setFillColorMsg color
    pushNamedMessage "right" msg obj