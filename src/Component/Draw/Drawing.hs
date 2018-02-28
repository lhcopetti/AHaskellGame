{-# LANGUAGE RecordWildCards #-}
module Component.Draw.Drawing
    ( Drawing (..)
    , updateDrawing
    , syncDrawing
    ) where

import SFML.Graphics.RenderWindow (drawCircle, drawRectangle, drawConvexShape, drawText, drawSprite)
import SFML.SFResource (destroy)

import qualified Component.Position as Pos
import Component.Draw.DrawingSync (executeUpdateOnDrawing)
import Drawable
import NativeResource
import System.Messaging.DrawingMessage
import GameObject.GameObjectTypes (GameObject (..), Drawing (..), DrawingFlag (..))
import Component.Draw.Animation.AnimationDrawing (updateAnimation)

instance Drawable Drawing where
    draw wnd (CircleDrawing ptr)         = drawCircle wnd ptr Nothing
    draw wnd (RectangleDrawing ptr)      = drawRectangle wnd ptr Nothing
    draw wnd (ConvexDrawing ptr)         = drawConvexShape wnd ptr Nothing
    draw wnd (TextDrawing ptr)           = drawText wnd ptr Nothing
    draw wnd (SpriteDrawing sprite _)    = drawSprite wnd sprite Nothing
    draw wnd (AnimationDrawing _ sprite) = drawSprite wnd sprite Nothing
    draw wnd (FlaggedDrawing drawing _)  = draw wnd drawing
    draw wnd (CompositeDrawing drws)     = mapM_ (draw wnd) drws
    draw wnd (NamedDrawing _ drw)        = draw wnd drw
    draw wnd (PhysicsDebugDrawing drw _) = draw wnd drw
    draw _   EmptyDrawing                = return ()

updateDrawing :: GameObject -> GameObject
updateDrawing obj@GameObject{..} = obj { drawComp = update drawComp }

update :: Drawing -> Drawing
update (AnimationDrawing anim spr) = updateAnimation anim spr
update d = d

syncDrawing :: (Pos.Position a, DrawingInbox a) => Drawing -> a -> IO ()
syncDrawing (PhysicsDebugDrawing _ act) _ = act
syncDrawing (FlaggedDrawing drw flg) obj    = executeUpdateOnDrawing drw obj (updatePosition, updateRotation)
    where
        updatePosition = NoPositionUpdates `notElem` flg
        updateRotation = NoRotationUpdates `notElem` flg
syncDrawing (CompositeDrawing drws)  obj    = mapM_ (`syncDrawing` obj) drws
syncDrawing drw obj                         = executeUpdateOnDrawing drw obj (True, True)

instance NativeResource Drawing where
    free (CircleDrawing       ptr )  = destroy ptr
    free (RectangleDrawing    ptr )  = destroy ptr
    free (ConvexDrawing       ptr )  = destroy ptr
    free (TextDrawing         ptr )  = destroy ptr
    free (SpriteDrawing   spr tex )  = destroy spr >> destroy tex
    free (AnimationDrawing anim _)   = free anim
    free (CompositeDrawing    drws)  = mapM_ free drws
    free (FlaggedDrawing    ptr _ )  = free ptr
    free (NamedDrawing      _ ptr )  = free ptr
    free (PhysicsDebugDrawing drw _) = free drw
    free EmptyDrawing                = return ()