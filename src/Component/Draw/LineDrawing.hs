module Component.Draw.LineDrawing
    ( createLine
    ) where

import SFML.Graphics.ConvexShape
import SFML.Graphics.Color (Color)
import SFML.System.Vector2 (Vec2f (..))

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Component.Draw.Drawing
import Component.Draw.DrawingHelper (createShapeT)
import Component.Draw.ConvexHelper (setPoints)
import Math.Line (getRectFromLine)
        
createLine :: (Vec2f, Vec2f) -> Float -> Color -> MaybeT IO Drawing
createLine line thickness color = do 
    liftIO $ putStrLn $ "Creating line at " ++ show line ++ " T: " ++ show thickness ++ " C: " ++ show color
    lineShape <- createShapeT createConvexShape
    liftIO $ do 
        setPoints    lineShape (getRectFromLine line thickness)
        setFillColor lineShape color
        return (ConvexDrawing lineShape)