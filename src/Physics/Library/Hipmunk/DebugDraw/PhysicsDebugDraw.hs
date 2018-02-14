module Physics.Library.Hipmunk.DebugDraw.PhysicsDebugDraw
    ( mkDrawingFromShape
    , syncPhysicsDrawing
    ) where

import SFML.Graphics.Color

import qualified Physics.Hipmunk as H

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.StateVar

import GameObject.GameObjectTypes
import Physics.Library.Hipmunk.VectorConversion (hVectorToVec2f)
import System.Messaging.Handler.RunMessageHandler (runMessageT)
import System.Messaging.Messages.TransformableMessage (setPositionMsg, setRotationMsg)
import System.Messaging.Messages.ShapeMessage (setFillColorMsg)
import Component.Draw.CircleDrawing (createCenteredCircle)
import Component.Draw.ConvexDrawing (createConvex)
import Component.Draw.LineDrawing (createLine)


mkDrawingFromShape :: H.ShapeType -> MaybeT IO Drawing
mkDrawingFromShape (H.Circle radius) = do
    drw <- createCenteredCircle (realToFrac radius) white
    liftIO $ runMessageT (setFillColorMsg red) drw
    return drw
mkDrawingFromShape (H.Polygon points) = createConvex white (map hVectorToVec2f points)
mkDrawingFromShape (H.LineSegment s e t) = createLine segment thickness color
        where
            segment     = (hVectorToVec2f s, hVectorToVec2f e)
            thickness   = realToFrac t * 2 -- Chipmunk's thickness counts both ways
            color       = white

syncPhysicsDrawing :: H.Body -> H.Shape -> Drawing -> IO ()
syncPhysicsDrawing body _ drawing = do
    pos <- liftM hVectorToVec2f (get . H.position   $ body)
    ang <- liftM realToFrac     (get . H.angle      $ body)
    runMessageT (setRotationMsg ang) drawing
    runMessageT (setPositionMsg pos) drawing