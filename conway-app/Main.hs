module Main where

import SFML.Window
import SFML.Graphics.RenderWindow
import SFML.Graphics.Color

import Control.Monad.Trans.Maybe (MaybeT (..))
import Physics.PhysicsWorld (createWorld, initPhysicsLibrary)
import Physics.PhysicsTypes (PhysicsWorld)

import GameEnv (GameEnvironment (..), createGameEnv)
import GameObject.GameObject (GameObject)
import GameObject.AnyGameObject (AnyGameObject (..))
import ObjectsFactory
import System.GameSystem (startGame)
import System.GameWorld (GameWorld (..), GameScene (..))
import Vec2.Vec2Math

defaultGravity :: Float
defaultGravity = 30

main :: IO ()
main = do
    initPhysicsLibrary
    physicsWorld <- createWorld defaultGravity
    desktopMode <- getDesktopMode
    fsModes <- getFullscreenModes

    putStrLn $ "Current desktop Mode is: " ++ show desktopMode
    putStrLn ""
    putStrLn "Fullscreen modes: "
    putStrLn ""

    mapM_ (\m -> print m >> putStrLn "") fsModes

    let ctxSettings = Just $ ContextSettings 24 8 0 1 2 [ContextDefault]
    wnd <- createRenderWindow (VideoMode 640 480 32) "Conway's Game of Life" [SFDefaultStyle] ctxSettings
    
    -- Game Environment initialization
    dimensions <- getWindowSize wnd
    let gameEnv = createGameEnv dimensions

    objects  <- runMaybeT (createObjects gameEnv physicsWorld)
    case objects of 
        Nothing -> putStrLn "Error creating game objects"
        Just balls -> do
            let anyBalls = map AGO balls
            let world = GameWorld wnd
            let scene = GameScene physicsWorld anyBalls
            startGame world scene gameEnv 
            putStrLn "This is the End!"

createObjects :: GameEnvironment -> PhysicsWorld -> MaybeT IO [GameObject]
createObjects _ _ = do 
    let sqPositions =   map (addVec2f (Vec2f 5 5)) 
                        [ Vec2f 0   0
                        , Vec2f 50  0
                        , Vec2f 100 0
                        , Vec2f 0   50
                        , Vec2f 50  50
                        , Vec2f 100 50
                        , Vec2f 0   100
                        , Vec2f 50  100
                        , Vec2f 100 100
                        ]
    mapM (createSquareObject 40 white) sqPositions