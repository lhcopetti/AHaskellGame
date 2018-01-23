module Component.Draw.TriangleDrawing
    ( createEqTriangle
    ) where

import SFML.Graphics.ConvexShape
import SFML.Graphics.Color (Color)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Component.Draw.Drawing
import Component.Draw.DrawingHelper (createShapeT)
import Component.Draw.ConvexHelper (setPoints)
import Math.Triangle (getUnitEqPointsScaled)
        
createEqTriangle :: Float -> Color -> MaybeT IO Drawing
createEqTriangle size color = do 
    liftIO $ putStrLn $ "Creating Equilateral triangle S: " ++ show size ++ "C: " ++ show color
    eqT <- createShapeT createConvexShape
    liftIO $ do 
        setPoints eqT (getUnitEqPointsScaled size)
        return (ConvexDrawing eqT)