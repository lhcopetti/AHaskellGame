module GameEnv 
    ( GameEnvironment (..)
    , createGameEnv
    )
    where

import SFML.System.Vector2 (Vec2u)

import Input.Mouse (MouseInput (..))
import Vec2.Vec2Math (zero)

data GameEnvironment = GameEnvironment { gameArea :: Vec2u
                                       , score :: Integer
                                       , input :: MouseInput
                                       , countGOs :: Integer
                                       }


createGameEnv :: Vec2u -> GameEnvironment
createGameEnv gameArea = GameEnvironment gameArea 0 (MouseInput zero) 0

initialScore :: Integer
initialScore = 0