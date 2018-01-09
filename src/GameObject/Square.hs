{-# LANGUAGE NamedFieldPuns #-}
module GameObject.Square
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
import Synchronizable
import Drawable
import Killable
import GameEnv
import Behavior.BoxedBehavior (wrapAround)
import qualified Component.Position as Pos
import qualified Component.Physics  as Phy

data Square = Square { pointer  :: RectangleShape
                     , position :: Vec2f
                     , velocity :: Vec2f
                     , color    :: Color
                     , alive    :: Bool
                     }

instance Updatable Square where
    update s@Square { position, velocity } = do
        let newS = Pos.setPosition s (addVec2f position velocity)
        
        dimension <- asks gameArea
        let newSquare = wrapAround newS dimension

        return newSquare

instance Synchronizable Square where
    synchronize square = setPosition (pointer square) (position square)

instance Drawable Square where 
    draw wnd Square { pointer } = drawRectangle wnd pointer Nothing 

instance Pos.Position Square where
    getPosition = position
    setPosition square newPos = square { position = newPos }

instance Phy.Physics Square where
    getVelocity = velocity
    setVelocity square newVel = square { velocity = newVel }

instance Killable Square where
    isAlive = alive
    die s = s { alive = False }
    destroyResource = destroy . pointer

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
            return (Square r pos vel color True)