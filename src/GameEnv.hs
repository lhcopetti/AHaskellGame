module GameEnv 
    where

import SFML.System.Vector2 (Vec2u)

data GameEnvironment = GameEnvironment { gameArea :: Vec2u
                                       , score :: Integer
                                       }