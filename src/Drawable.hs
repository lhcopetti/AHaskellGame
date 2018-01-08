module Drawable
    ( DrawType
    , Drawable
    , draw
    ) where

import SFML.Graphics.Types (RenderWindow)

type DrawType a = RenderWindow -> a -> IO ()

class Drawable a where
    draw :: DrawType a