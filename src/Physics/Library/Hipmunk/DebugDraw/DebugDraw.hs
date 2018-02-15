module Physics.Library.Hipmunk.DebugDraw.DebugDraw
    ( mkDebugDraw
    ) where

import Control.Monad.Trans.Maybe (MaybeT)

import GameObject.GameObjectTypes
import Physics.Library.Hipmunk.DebugDraw.PhysicsDebugDraw (mkDrawingFromShape, syncPhysicsDrawing)
import Physics.Library.Hipmunk.PhysicsTypes


mkDebugDraw :: Physics -> MaybeT IO Drawing
mkDebugDraw SimplePhy {} = error "Not prepared to handle 'SimplePhy'"
mkDebugDraw (HipPhy (PL body shape shapeType _)) = do
    drawing <- mkDrawingFromShape shapeType
    let action = syncPhysicsDrawing body shape drawing
    return (PhysicsDebugDrawing drawing action)