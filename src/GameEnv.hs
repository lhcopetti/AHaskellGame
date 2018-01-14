module GameEnv 
    where

import SFML.System.Vector2 (Vec2u)

import Input.Mouse (MouseInput)

data GameEnvironment = GameEnvironment { gameArea :: Vec2u
                                       , score :: Integer
                                       , input :: MouseInput
                                       }