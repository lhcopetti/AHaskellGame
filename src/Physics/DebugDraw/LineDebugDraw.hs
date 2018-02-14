module Physics.DebugDraw.LineDebugDraw
    ( mkLineDebugDraw
    ) where

import SFML.Graphics.Color
import SFML.System.Vector2 (Vec2f (..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)

import GameObject.GameObjectTypes
import System.Messaging.Handler.RunMessageHandler (runMessageT)
import System.Messaging.Messages.ShapeMessage
import Component.Draw.LineDrawing (createLine)
import Physics.DebugDraw.DefaultParams


mkLineDebugDraw :: (Vec2f, Vec2f) -> Float -> MaybeT IO Drawing
mkLineDebugDraw line thickness = do
    drw <- createLine line thickness lightColor
    liftIO $ do
        runMessageT (setOutlineThicknessMsg lightThickness) drw
        runMessageT (setOutlineColorMsg green) drw
    return drw