{-# LANGUAGE InstanceSigs #-}

module Ball 
    ( Ball (..)
    , createBall
    , drawBall
    , update
    ) where
    
import SFML.Graphics.Color
import SFML.Graphics.CircleShape
import SFML.System.Vector2
import Vec2.Vec2Math (zero, addVec2f)
import SFML.Graphics.Types
import SFML.Graphics.RenderWindow (drawCircle)
import SFML.Graphics.SFShape (setFillColor)
import SFML.Graphics.SFTransformable (setPosition)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Control.Monad (mzero)
import GameEnv
import Updatable

data Ball = Ball { circle   :: CircleShape
                 , position :: Vec2f
                 , velocity :: Vec2f
                 , color    :: Color
                 }

instance Show Ball where
    show (Ball c pos vel col) = "This is a ball of [" ++ show col ++ "] color"

instance Updatable Ball where

    update :: Ball -> ReaderT GameEnvironment IO Ball
    update b@(Ball c pos vel@(Vec2f velX velY) color) = do
        (Vec2u width height) <- asks gameArea
        let newPos@(Vec2f x y) = addVec2f pos vel
    
        let newVelX = if x > fromIntegral width || x < 0 then (-velX) else velX
        let newVelY = if y > fromIntegral height || y < 0 then (-velY) else velY
    
        liftIO (setPosition c newPos)
        return (Ball c newPos (Vec2f newVelX newVelY) color)


createBall :: Vec2f -> Vec2f -> MaybeT IO Ball
createBall pos@(Vec2f x y) vel = do 
    liftIO $ putStrLn $ "Creating ball at " ++ show pos
    myCircle <- liftIO createCircleShape
    case myCircle of
        Left e -> do 
            liftIO (putStrLn $ "Error while trying to create a circle shape. " ++ show e)
            mzero
        Right r -> do
            let color = blue
            liftIO $ setFillColor r color
            liftIO $ setRadius r 25
            return (Ball r pos vel color)

drawBall :: RenderWindow -> Ball -> IO ()
drawBall wnd (Ball circle pos vel color) = drawCircle wnd circle Nothing