module Input.Mouse 
    ( MouseInput (..)
    , getMouseInput
    ) where

import SFML.System.Vector2 (Vec2f (..), Vec2i (..))
import SFML.Graphics.Types (RenderWindow)
import SFML.Graphics.RenderWindow (getMousePosition)

data MouseInput = MouseInput { mousePos :: Vec2f
                             }

newMouseInput :: Vec2i -> MouseInput
newMouseInput (Vec2i px py) = let 
    fx = fromIntegral px
    fy = fromIntegral py
    in 
        MouseInput (Vec2f fx fy)

getMouseInput :: RenderWindow -> IO MouseInput
getMouseInput wnd = do
    mousePosition <- getMousePosition (pure wnd)
    return (newMouseInput mousePosition)
