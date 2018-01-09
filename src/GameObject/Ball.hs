{-# LANGUAGE InstanceSigs #-}

module GameObject.Ball 
    ( Ball (..)
    , createBall
    , draw
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
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Control.Monad (mzero)

import GameEnv
import Updatable
import Synchronizable
import Drawable
import qualified Component.Position as Pos
import qualified Component.Physics as Phy
import Behavior.BoxedBehavior (boundToDimension)

data Ball = Ball { circle   :: CircleShape
                 , position :: Vec2f
                 , velocity :: Vec2f
                 , color    :: Color
                 }

instance Show Ball where
    show (Ball c pos vel col) = "This is a ball of [" ++ show col ++ "] color"

instance Updatable Ball where
    update b@(Ball c pos vel@(Vec2f velX velY) color) = do
        -- Physics update
        let newB = Pos.setPosition b (addVec2f pos vel)

        -- Behavior update
        dimension <- asks gameArea
        let newBall = boundToDimension newB dimension

        -- Pointer update
        liftIO (setPosition c (Pos.getPosition newBall))

        -- Return the new object
        return newBall

instance Synchronizable Ball where
    synchronize x = return ()

instance Drawable Ball where 

    draw :: RenderWindow -> Ball -> IO ()
    draw wnd (Ball circle pos vel color) = drawCircle wnd circle Nothing

instance Pos.Position Ball where
    getPosition = position
    setPosition ball newPosition = ball { position = newPosition } 

instance Phy.Physics Ball where
    getVelocity = velocity
    setVelocity ball newVel = ball { velocity = newVel }

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