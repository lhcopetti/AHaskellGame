module Component.Draw.CircleDrawing
    ( createCircle
    , createCenteredCircle
    ) where

import SFML.Graphics.CircleShape (createCircleShape, setRadius, setOrigin)
import SFML.Graphics.Color (Color)
import SFML.Graphics.SFShape (setFillColor)
import SFML.System.Vector2 (Vec2f (..))

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

createCenteredCircle :: Float -> Color -> MaybeT IO Drawing
createCenteredCircle radius color = do 
    circleDrawing <- createCircle radius color
    liftIO $ setOriginDrawing circleDrawing newOrigin
    return circleDrawing
        where
            newOrigin = Vec2f radius radius