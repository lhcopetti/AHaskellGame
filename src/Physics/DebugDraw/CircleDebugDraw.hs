module Physics.DebugDraw.CircleDebugDraw
    ( mkCircleDebugDraw
    ) where

import SFML.Graphics.Color
import SFML.System.Vector2 (Vec2f (..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)

import GameObject.GameObjectTypes
import System.Messaging.Handler.RunMessageHandler (runMessageT)
import System.Messaging.Messages.ShapeMessage
import Component.Draw.CircleDrawing (createCenteredCircle)
import Component.Draw.LineDrawing (createLine)
import Component.Draw.CompositeDrawing (createComposite)
import Physics.DebugDraw.DefaultParams


mkCircleDebugDraw :: Float -> MaybeT IO Drawing
mkCircleDebugDraw radius = do
    drw <- createCenteredCircle radius lightColor
    liftIO $ do
        runMessageT (setOutlineThicknessMsg lightThickness) drw
        runMessageT (setOutlineColorMsg green) drw
    line <- createLine (Vec2f 0 0, Vec2f radius 0) 1 strongColor
    createComposite [drw, line]