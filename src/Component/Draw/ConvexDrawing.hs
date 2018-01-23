module Component.Draw.ConvexDrawing
    ( createConvex
    ) where

import SFML.Graphics.ConvexShape
import SFML.Graphics.Color (Color)
import SFML.System.Vector2 (Vec2f)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Component.Draw.Drawing
import Component.Draw.DrawingHelper (createShapeT)
import Component.Draw.ConvexHelper (setPoints)

createConvex :: Color -> [Vec2f] -> MaybeT IO Drawing
createConvex color points = do 
    liftIO $ putStrLn $ "Creating convexDrawing Points: " ++ show points
    convex <- createShapeT createConvexShape
    liftIO $ do 
        setPoints convex points
        setFillColor convex color
        return (ConvexDrawing convex)
    