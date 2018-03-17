{-# LANGUAGE RankNTypes #-}
module Component.Draw.Instances.SFShapeDrawing
    ( 
    ) where

import SFML.Graphics.CircleShape ()
import SFML.Graphics.RectangleShape ()
import SFML.Graphics.ConvexShape ()
import SFML.Graphics.Text ()
import SFML.Graphics.Sprite ()
import SFML.Graphics.Texture ()
import SFML.Graphics.SFShape
import SFML.Graphics.Color

import GameObject.GameObjectTypes
import Vec2.Vec2Math (zero)

instance SFShape Drawing where
    setFillColor    drw color = runOnShape (`setFillColor` color) drw
    setOutlineColor drw color = runOnShape (`setOutlineColor` color) drw
    setOutlineThickness drw f = runOnShape (`setOutlineThickness` f) drw

    -- You know the interface is really broken when you have to 
    -- implement things like these:
    getFillColor        _ = getFillColor        EmptyShape
    getOutlineColor     _ = getOutlineColor     EmptyShape
    getOutlineThickness _ = getOutlineThickness EmptyShape
    getPointCount       _ = getPointCount       EmptyShape
    getPoint            _ = getPoint            EmptyShape
    -- I will have it removed when I isolate the SFML library
    -- and redesign some aspects of the Drawing component


runOnShape :: (forall a. SFShape a => a -> IO ()) -> Drawing -> IO ()
runOnShape f (CircleDrawing ptr)         = f ptr
runOnShape f (RectangleDrawing  ptr)     = f ptr
runOnShape f (ConvexDrawing     ptr)     = f ptr
runOnShape f (FlaggedDrawing  drw _)     = runOnShape f drw
runOnShape f (NamedDrawing   _  drw)     = runOnShape f drw
runOnShape f (PhysicsDebugDrawing drw _) = runOnShape f drw
runOnShape f (CompositeDrawing drws)     = mapM_ (runOnShape f) drws
runOnShape _ TextDrawing {}              = return ()
runOnShape _ SpriteDrawing {}            = return ()
runOnShape _ AnimationDrawing {}         = return ()
runOnShape _ EmptyDrawing                = return ()

data EmptyShape = EmptyShape

instance SFShape EmptyShape where
    setFillColor        _ _ = return ()
    setOutlineColor     _ _ = return ()
    setOutlineThickness _ _ = return ()

    getFillColor          _ = return black
    getOutlineColor       _ = return black
    getOutlineThickness   _ = return 0.0
    getPointCount         _ = return 0
    getPoint            _ _ = return zero