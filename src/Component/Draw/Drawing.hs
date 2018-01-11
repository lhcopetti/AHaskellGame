module Component.Draw.Drawing
    ( Drawing (..)
    , updateDrawing
    , destroyDrawing
    ) where

import SFML.Graphics.Types (CircleShape)
import SFML.Graphics.RenderWindow (drawCircle)
import SFML.Graphics.SFTransformable
import SFML.Graphics.CircleShape
import SFML.SFResource

import qualified Component.Position as Pos
import Drawable

data Drawing = CircleDrawing CircleShape

instance Drawable Drawing where
    draw wnd (CircleDrawing ptr) = drawCircle wnd ptr Nothing


updateDrawing :: Pos.Position a => Drawing -> a -> IO ()
updateDrawing (CircleDrawing shape) obj = setPosition shape (Pos.getPosition obj)


destroyDrawing :: Drawing -> IO ()
destroyDrawing (CircleDrawing ptr) = destroy ptr