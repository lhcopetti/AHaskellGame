module GameEnv 
    ( GameEnvironment (..)
    , createGameEnv
    )
    where

import SFML.System.Vector2 (Vec2u)

import Input.Mouse (MouseInput (..))
import System.InputSnapshot (InputSnapshot, emptySnapshot)
import Vec2.Vec2Math (zero)

data GameEnvironment = GameEnvironment { gameArea :: Vec2u
                                       , score :: Integer
                                       , input :: MouseInput
                                       , inputSnapshot :: InputSnapshot
                                       , countGOs :: Integer
                                       }

createGameEnv :: Vec2u -> GameEnvironment
createGameEnv screenArea = GameEnvironment 
                                screenArea
                                initialScore
                                (MouseInput zero)  
                                emptySnapshot
                                0

initialScore :: Integer
initialScore = 0