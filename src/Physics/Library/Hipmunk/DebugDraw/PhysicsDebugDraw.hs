module Physics.Library.Hipmunk.DebugDraw.PhysicsDebugDraw
    ( mkDrawingFromShape
    , syncPhysicsDrawing
    ) where

import qualified Physics.Hipmunk as H

import Control.Monad (liftM)
import Control.Monad.Trans.Maybe (MaybeT)
import Data.StateVar

import GameObject.GameObjectTypes
import Physics.Library.Hipmunk.VectorConversion (hVectorToVec2f)
import Physics.DebugDraw.CircleDebugDraw (mkCircleDebugDraw)
import Physics.DebugDraw.PolygonDebugDraw (mkPolygonDebugDraw)
import Physics.DebugDraw.LineDebugDraw (mkLineDebugDraw)
import System.Messaging.Handler.RunMessageHandler (runMessageT)
import System.Messaging.Messages.TransformableMessage


mkDrawingFromShape :: H.ShapeType -> MaybeT IO Drawing
mkDrawingFromShape (H.Circle radius) = mkCircleDebugDraw (realToFrac radius)
mkDrawingFromShape (H.Polygon points) = mkPolygonDebugDraw (map hVectorToVec2f points)
mkDrawingFromShape (H.LineSegment s e t) = mkLineDebugDraw segment thickness
        where
            segment     = (hVectorToVec2f s, hVectorToVec2f e)
            thickness   = realToFrac t * 2 -- Chipmunk's thickness counts both ways

syncPhysicsDrawing :: H.Body -> H.Shape -> Drawing -> IO ()
syncPhysicsDrawing body _ drawing = do
    pos <- liftM hVectorToVec2f (get . H.position   $ body)
    ang <- liftM realToFrac     (get . H.angle      $ body)
    runMessageT (setRotationMsg ang) drawing
    runMessageT (setPositionMsg pos) drawing