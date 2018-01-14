module Input.Mouse 
    ( MouseInput (..)
    ) where

import SFML.System.Vector2 (Vec2f)

data MouseInput = MouseInput { position :: Vec2f
                   }