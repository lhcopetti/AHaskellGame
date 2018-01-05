module Drawable where

import SFML.Graphics.Types (RenderWindow)

class Drawable a where
    draw :: RenderWindow -> a -> IO ()