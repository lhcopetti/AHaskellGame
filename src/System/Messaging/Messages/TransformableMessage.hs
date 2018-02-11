module System.Messaging.Messages.TransformableMessage
    ( setOriginMsg
    , setPositionMsg
    , setRotationMsg
    ) where

import SFML.System.Vector2 (Vec2f)
import SFML.Graphics.CircleShape ()
import SFML.Graphics.RectangleShape ()
import SFML.Graphics.ConvexShape ()
import SFML.Graphics.Text ()
import SFML.Graphics.Sprite ()
import SFML.Graphics.Texture ()
import SFML.Graphics.SFTransformable (setOrigin, setPosition, setRotation)

import GameObject.GameObjectTypes (Drawing (..), DrawingMessageType)

setOriginMsg :: Vec2f -> DrawingMessageType
setOriginMsg pos (CircleDrawing     ptr)    = setOrigin    ptr pos
setOriginMsg pos (RectangleDrawing  ptr)    = setOrigin    ptr pos
setOriginMsg pos (ConvexDrawing     ptr)    = setOrigin    ptr pos
setOriginMsg pos (TextDrawing       ptr)    = setOrigin    ptr pos
setOriginMsg pos (SpriteDrawing   ptr _)    = setOrigin    ptr pos
setOriginMsg pos (AnimationDrawing _ ptr)   = setOrigin    ptr pos
setOriginMsg pos (FlaggedDrawing  ptr _)    = setOriginMsg pos ptr
setOriginMsg pos (NamedDrawing   _  drw)    = setOriginMsg pos drw
setOriginMsg pos (CompositeDrawing drws)    = mapM_ (pos `setOriginMsg`) drws
setOriginMsg _   PhysicsDebugDrawing {}     = return ()

setPositionMsg :: Vec2f -> DrawingMessageType
setPositionMsg pos (CircleDrawing     ptr)    = setPosition    ptr pos
setPositionMsg pos (RectangleDrawing  ptr)    = setPosition    ptr pos
setPositionMsg pos (ConvexDrawing     ptr)    = setPosition    ptr pos
setPositionMsg pos (TextDrawing       ptr)    = setPosition    ptr pos
setPositionMsg pos (SpriteDrawing   ptr _)    = setPosition    ptr pos
setPositionMsg pos (AnimationDrawing _ ptr)   = setPosition    ptr pos
setPositionMsg pos (FlaggedDrawing  ptr _)    = setPositionMsg pos ptr
setPositionMsg pos (NamedDrawing   _  drw)    = setPositionMsg pos drw
setPositionMsg pos (CompositeDrawing drws)    = mapM_ (pos `setPositionMsg`) drws
setPositionMsg _   PhysicsDebugDrawing {}     = return ()

setRotationMsg :: Float -> DrawingMessageType
setRotationMsg ang (CircleDrawing     ptr)    = setRotation    ptr ang
setRotationMsg ang (RectangleDrawing  ptr)    = setRotation    ptr ang
setRotationMsg ang (ConvexDrawing     ptr)    = setRotation    ptr ang
setRotationMsg ang (TextDrawing       ptr)    = setRotation    ptr ang
setRotationMsg ang (SpriteDrawing   ptr _)    = setRotation    ptr ang
setRotationMsg ang (AnimationDrawing _ ptr)   = setRotation    ptr ang
setRotationMsg ang (FlaggedDrawing  ptr _)    = setRotationMsg ang ptr
setRotationMsg ang (NamedDrawing   _  drw)    = setRotationMsg ang drw
setRotationMsg ang (CompositeDrawing drws)    = mapM_ (ang `setRotationMsg`) drws
setRotationMsg _   PhysicsDebugDrawing {}     = return ()