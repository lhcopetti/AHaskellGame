module Physics.DebugDraw.PolygonDebugDraw
    ( mkPolygonDebugDraw
    ) where

import SFML.Graphics.Color
import SFML.System.Vector2 (Vec2f (..))

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad (MonadPlus, guard)

import GameObject.GameObjectTypes
import System.Messaging.Handler.RunMessageHandler (runMessageT)
import System.Messaging.Messages.ShapeMessage
import Component.Draw.ConvexDrawing (createConvex)
import Component.Draw.LineDrawing (createLine)
import Component.Draw.CompositeDrawing (createComposite)
import Physics.DebugDraw.DefaultParams

import Vec2.Vec2Math (midPoint)


mkPolygonDebugDraw :: [Vec2f] -> MaybeT IO Drawing
mkPolygonDebugDraw points = do
    drw <- createConvex lightColor points
    liftIO $ do
        runMessageT (setOutlineThicknessMsg lightThickness) drw
        runMessageT (setOutlineColorMsg green) drw
    midPoint <- getMidPointLine points
    line <- createLine (Vec2f 0 0, midPoint) 1 strongColor
    createComposite [drw, line]

getMidPointLine :: MonadPlus m => [Vec2f] -> m Vec2f
getMidPointLine points = do
    guard (length points > 2)
    let v1 = head points
        v2 = points !! 1
    return (midPoint (v1, v2))