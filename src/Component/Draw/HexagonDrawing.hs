module Component.Draw.HexagonDrawing
    ( createHexagon
    ) where

import SFML.Graphics.Types (ConvexShape)
import SFML.Graphics.ConvexShape
import SFML.Graphics.Color (Color)
import SFML.System.Vector2 (Vec2f)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Component.Draw.Drawing
import Component.Draw.DrawingHelper (createShapeT)
import Component.Draw.ConvexHelper (setPoints)
import Math.Hexagon (getUnitHexPointsScaled)
        
createHexagon :: Float -> Color -> MaybeT IO Drawing
createHexagon size color = do 
    liftIO $ putStrLn $ "Creating hexagon S: " ++ show size ++ "C: " ++ show color
    hex <- createShapeT createConvexShape
    liftIO $ do 
        setPoints hex (getUnitHexPointsScaled size)
        return (ConvexDrawing hex)