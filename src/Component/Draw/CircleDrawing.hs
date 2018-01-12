module Component.Draw.CircleDrawing
    ( createCircle
    ) where

import SFML.Graphics.CircleShape (createCircleShape, setRadius)
import SFML.Graphics.Color (Color)
import SFML.Graphics.SFShape (setFillColor)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Component.Draw.Drawing
import Component.Draw.DrawingHelper (createShapeT)

createCircle :: Float -> Color -> MaybeT IO Drawing
createCircle radius color = do 
    liftIO $ putStrLn $ "Creating circle R: " ++ show radius ++ "C: " ++ show color
    circle <- createShapeT createCircleShape
    liftIO $ do 
        setFillColor circle color
        setRadius circle radius
        return (CircleDrawing circle)