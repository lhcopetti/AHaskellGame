{-# LANGUAGE RankNTypes #-}
module System.Messaging.Messages.ShapeMessage
    ( setFillColorMsg
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

setFillColorMsg :: Color -> DrawingMessageType
setFillColorMsg c = (`setFillColor` c)

-- setPositionMsg :: Vec2f -> DrawingMessageType
-- setPositionMsg pos = runOnTransformable (`setOrigin` pos)

-- setRotationMsg :: Float -> DrawingMessageType
-- setRotationMsg ang = runOnTransformable (`setRotation` ang)

instance SFShape Drawing where
    setFillColor    drw color = runOnShape (`setFillColor` color) drw
    setOutlineColor drw color = runOnShape (`setOutlineColor` color) drw
    setOutlineThickness drw f = runOnShape (`setOutlineThickness` f) drw

    -- I am aware that this transfer the error that should happen at compile time
    -- to one that will explore during runtime. Hovewer, I still have no idea
    -- what the proper abstraction for this behavior should be.
    getFillColor        = undefined
    getOutlineColor     = undefined
    getOutlineThickness = undefined
    getPointCount       = undefined
    getPoint            = undefined

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