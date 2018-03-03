{-# LANGUAGE CPP #-}
module Main where

import SFML.Window
import SFML.Graphics.RenderWindow
import SFML.Graphics.Color

import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State
import Physics.PhysicsWorld (createWorld, initPhysicsLibrary)
import Physics.PhysicsTypes (PhysicsWorld)
import System.Messaging.Messages.ShapeMessage
import System.Messaging.Handler.PushMessageHandler

import GameEnv (GameEnvironment (..), createGameEnv)
import GameObject.GameObject (GameObject)
import GameObject.AnyGameObject (AnyGameObject (..))
import GameObject.GameObjectTypes
import ObjectsFactory
import System.GameSystem (startGame)
import System.GameWorld (GameWorld (..), GameScene (..))
import Vec2.Vec2Math
import Conway

defaultGravity :: Float
defaultGravity = 30

main :: IO ()
main = do
#ifdef DO_SOMETHING_DIFFERENT
    putStrLn "Doing something different!"
#endif
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
        Just balls -> case newConwayWorld (3, 3) of
                        Just b -> do
                            let n = setLive (1, 0) b
                                n' = setLive (1, 1) n
                                n'' = setLive (1, 2) n'
                            let anyBalls = map AGO balls
                            let world = GameWorld wnd
                            let scene = GameScene physicsWorld anyBalls n''
                            startGame world scene gameEnv 
                            putStrLn "This is the End!"
                        Nothing -> do
                            putStrLn "Error creating conway board"
                            return ()

createObjects :: GameEnvironment -> PhysicsWorld -> MaybeT IO [GameObject]
createObjects _ _ = do 
    let drawingPos =   map (addVec2f (Vec2f 5 5)) 
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
        cellPos = [(x, y) | y <- [0..2], x <- [0..2]]
    objs <- mapM createConwayCell (zip drawingPos cellPos)
    logicGO <- createLogicGO (Behavior stepConway)
    return (objs ++ [logicGO])


createConwayCell :: (Vec2f, Position) -> MaybeT IO GameObject
createConwayCell (pos, idx) = do
    obj <- createSquareObject 40 white pos
    setBehaviorFor (setConwayColorBehavior idx) obj


stepConway :: BehaviorType
stepConway go = do
    modify tick
    return go


setBehaviorFor :: Monad m => BehaviorType -> GameObject -> m GameObject
setBehaviorFor bt go = return $ go { behavior = Behavior bt }

setConwayColorBehavior :: Position -> BehaviorType
setConwayColorBehavior pos go = do
    isLive <- gets (isLive pos)
    let color = if isLive then white else red
    pushMessage (setFillColorMsg color) go