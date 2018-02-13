{-# LANGUAGE RankNTypes #-}
module Component.Draw.Instances.SFTransformableDrawing
    (
    ) where

import SFML.Graphics.CircleShape ()
import SFML.Graphics.RectangleShape ()
import SFML.Graphics.ConvexShape ()
import SFML.Graphics.Text ()
import SFML.Graphics.Sprite ()
import SFML.Graphics.Texture ()
import SFML.Graphics.SFTransformable

import Control.Monad (liftM)
import qualified Data.List.NonEmpty as LNE

import GameObject.GameObjectTypes

instance SFTransformable Drawing where
    setPosition drw v   = runOnTransformable (`setPosition` v) drw
    setRotation drw a   = runOnTransformable (`setRotation` a) drw
    setScale    drw s   = runOnTransformable (`setScale`    s) drw
    setOrigin   drw o   = runOnTransformable (`setOrigin`   o) drw
    move        drw p   = runOnTransformable (`move`        p) drw
    rotate      drw a   = runOnTransformable (`rotate`      a) drw
    scale       drw s   = runOnTransformable (`scale`       s) drw

    getPosition         = runOnTransformable getPosition
    getOrigin           = runOnTransformable getOrigin
    getScale            = runOnTransformable getScale
    getRotation         = runOnTransformable getRotation
    getTransform        = runOnTransformable getTransform
    getInverseTransform = runOnTransformable getInverseTransform


runOnTransformable :: (forall a. SFTransformable a => a -> IO b) -> Drawing -> IO b
runOnTransformable f (CircleDrawing ptr)         = f ptr
runOnTransformable f (RectangleDrawing  ptr)     = f ptr
runOnTransformable f (ConvexDrawing     ptr)     = f ptr
runOnTransformable f (TextDrawing       ptr)     = f ptr
runOnTransformable f (SpriteDrawing   ptr _)     = f ptr
runOnTransformable f (AnimationDrawing _ ptr)    = f ptr
runOnTransformable f (PhysicsDebugDrawing ptr _) = f ptr
runOnTransformable f (FlaggedDrawing  drw _)     = runOnTransformable f drw
runOnTransformable f (NamedDrawing   _  drw)     = runOnTransformable f drw
runOnTransformable f (CompositeDrawing drws)     = liftM LNE.head $ mapM (runOnTransformable f) drws