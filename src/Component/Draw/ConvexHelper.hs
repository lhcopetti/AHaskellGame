module Component.Draw.ConvexHelper
    ( setPoints
    ) where

import SFML.Graphics.Types (ConvexShape)
import SFML.Graphics.ConvexShape
import SFML.System.Vector2 (Vec2f)

import Control.Monad (forM_)

setPoints :: ConvexShape -> [Vec2f] -> IO ()
setPoints ptr points = do
    setPointCount ptr (length points)
    forM_ (zip [0..] points) (setConvexPoint ptr)

setConvexPoint :: ConvexShape -> (Int, Vec2f) -> IO ()
setConvexPoint ptr (idx, point) = setPoint ptr idx point