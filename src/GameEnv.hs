{-# LANGUAGE NamedFieldPuns #-}
module GameEnv 
    ( GameEnvironment (..)
    , GameTime (..)
    , createGameEnv
    , updateGameEnv
    )
    where

import SFML.System.Vector2 (Vec2u)

import Input.Mouse (MouseInput (..))
import System.InputSnapshot (InputSnapshot, emptySnapshot)
import Vec2.Vec2Math (zero)
import Data.Time

data GameEnvironment = GameEnvironment { gameArea :: Vec2u
                                       , score :: Integer
                                       , input :: MouseInput
                                       , inputSnapshot :: InputSnapshot
                                       , countGOs :: Integer
                                       , time :: GameTime
                                       }

data GameTime = GameTime { startTime    :: UTCTime
                         , runningTime  :: NominalDiffTime
                         }

setRunningTime :: UTCTime -> GameTime -> GameTime
setRunningTime utc (GameTime st _) = let
    diff = diffUTCTime utc st
    in
        GameTime st diff

createGameEnv :: Vec2u -> UTCTime -> GameEnvironment
createGameEnv screenArea time = GameEnvironment 
                                screenArea
                                initialScore
                                (MouseInput zero)  
                                emptySnapshot
                                0
                                (GameTime time 0)

initialScore :: Integer
initialScore = 0

updateGameEnv :: GameEnvironment -> MouseInput -> Integer -> InputSnapshot -> IO GameEnvironment
updateGameEnv env@GameEnvironment { time } mouse liveGameObjects snapshot = do
    now <- getCurrentTime
    return $ env    
            { input = mouse
            , countGOs = liveGameObjects
            , inputSnapshot = snapshot
            , time = setRunningTime now time
            } 