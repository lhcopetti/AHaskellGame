module Component.Draw.RectangleDrawing
    ( createRectangle
    , createSquare
    ) where

import SFML.Graphics.Types (RectangleShape)
import SFML.Graphics.RectangleShape (createRectangleShape, setSize)
import SFML.Graphics.Color (Color)
import SFML.Graphics.SFShape (setFillColor)
import SFML.System.Vector2 (Vec2f (..))

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Component.Draw.Drawing
import Component.Draw.DrawingHelper (createShapeT)

createRectangle :: Vec2f -> Color -> MaybeT IO Drawing
createRectangle size color = do 
    liftIO $ putStrLn $ "Creating rectangle S: " ++ show size ++ "C: " ++ show color
    rect <- createShapeT createRectangleShape
    liftIO $ do 
        setFillColor rect color
        setSize rect size
        return (RectangleDrawing rect)

createSquare :: Float -> Color -> MaybeT IO Drawing
createSquare x = createRectangle (Vec2f x x)