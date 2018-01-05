{-# LANGUAGE InstanceSigs #-}

module Square
    ( Square(..)
    , createSquare
    , update
    , draw
    ) where

import SFML.Graphics.Color
import SFML.System.Vector2
import SFML.Graphics.RectangleShape
import SFML.Graphics.Types (RectangleShape, RenderWindow)
import SFML.Graphics.RenderWindow

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)

import Vec2.Vec2Math (zero, addVec2f)
import Updatable
import Drawable
import GameEnv

data Square = Square { circle   :: RectangleShape
                     , position :: Vec2f
                     , velocity :: Vec2f
                     , color    :: Color
                     }

instance Updatable Square where

    update :: Square -> ReaderT GameEnvironment IO Square
    update s@(Square c pos vel@(Vec2f velX velY) color) = do
        (Vec2u width height) <- asks gameArea
        let newPos@(Vec2f x y) = addVec2f pos vel
    
        let newVelX = if x > fromIntegral width || x < 0 then (-velX) else velX
        let newVelY = if y > fromIntegral height || y < 0 then (-velY) else velY
    
        liftIO (setPosition c newPos)
        return (Square c newPos (Vec2f newVelX newVelY) color)


instance Drawable Square where 

    draw :: RenderWindow -> Square -> IO ()
    draw wnd (Square rect pos vel color) = drawRectangle wnd rect Nothing 


createSquare :: Vec2f -> Vec2f -> MaybeT IO Square
createSquare pos@(Vec2f x y) vel = do 
    liftIO $ putStrLn $ "Creating square at " ++ show pos
    mySquare <- liftIO createRectangleShape
    case mySquare of
        Left e -> do 
            liftIO (putStrLn $ "Error while trying to create a rectangle shape. " ++ show e)
            mzero
        Right r -> do
            let color = green
            liftIO $ setFillColor r color
            liftIO $ setSize r (Vec2f 25 25)
            return (Square r pos vel color)