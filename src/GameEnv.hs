{-# LANGUAGE NamedFieldPuns #-}
module GameEnv 
    ( GameEnvironment (..)
    , GameTime (..)
    , createGameEnv
    , updateGameEnv
    , updateCollisionData
    )
    where

import SFML.System.Vector2 (Vec2u)

import Input.Mouse (MouseInput (..))
import System.Input.InputSnapshot (InputSnapshot, emptySnapshot)
import Vec2.Vec2Math (zero)
import Data.Time
import Physics.PhysicsTypes (PhyCollisionData)
import Physics.PhysicsCollision (emptyCollisionData)

data GameEnvironment = GameEnvironment { gameArea :: Vec2u
                                       , score :: Integer
                                       , input :: MouseInput
                                       , inputSnapshot :: InputSnapshot
                                       , countGOs :: Integer
                                       , time :: GameTime
                                       , collisionData :: PhyCollisionData
                                       }

data GameTime = GameTime { startTime    :: UTCTime
                         , runningTime  :: NominalDiffTime
                         , deltaTime    :: NominalDiffTime
                         }

setRunningTime :: UTCTime -> GameTime -> GameTime
setRunningTime utcTime (GameTime st _ dt) = let
    diff = diffUTCTime utcTime st
    in
        GameTime st diff dt

setDeltaTime :: NominalDiffTime -> GameTime -> GameTime
setDeltaTime dt g = g { deltaTime = dt }

createGameEnv :: Vec2u -> UTCTime -> GameEnvironment
createGameEnv screenArea time = GameEnvironment 
                                screenArea
                                initialScore
                                (MouseInput zero)  
                                emptySnapshot
                                0
                                (GameTime time 0 0)
                                emptyCollisionData

initialScore :: Integer
initialScore = 0

updateGameEnv :: GameEnvironment -> NominalDiffTime -> MouseInput -> Integer -> InputSnapshot -> IO GameEnvironment
updateGameEnv env@GameEnvironment { time } dt mouse liveGameObjects snapshot = do
    now <- getCurrentTime
    return $ env    
            { input = mouse
            , countGOs = liveGameObjects
            , inputSnapshot = snapshot
            , time = setDeltaTime dt . setRunningTime now $ time
            }

updateCollisionData :: PhyCollisionData -> GameEnvironment -> GameEnvironment
updateCollisionData newCollData g
    = g { collisionData = newCollData }