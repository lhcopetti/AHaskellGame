{-# LANGUAGE NamedFieldPuns #-}
module GameObject.Ball 
    ( Ball (..)
    , createBall
    , draw
    , update
    , synchronize
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
import Killable
import qualified Component.Position as Pos
import qualified Component.Physics as Phy
import Behavior.BoxedBehavior (boundToDimension)

data Ball = Ball { circle   :: CircleShape
                 , position :: Vec2f
                 , velocity :: Vec2f
                 , color    :: Color
                 , alive    :: Bool
                 }

instance Updatable Ball where
    update b@Ball { position, velocity } = do
        -- Physics update
        let newB = Pos.setPosition b (addVec2f position velocity)

        -- Behavior update
        dimension <- asks gameArea
        let newBall = boundToDimension newB dimension

        -- Return the new object
        return newBall

instance Synchronizable Ball where
    synchronize ball = setPosition (circle ball) (position ball)

instance Drawable Ball where 
    draw wnd Ball { circle } = drawCircle wnd circle Nothing

instance Pos.Position Ball where
    getPosition = position
    setPosition ball newPosition = ball { position = newPosition } 

instance Phy.Physics Ball where
    getVelocity = velocity
    setVelocity ball newVel = ball { velocity = newVel }

instance Killable Ball where 
    isAlive = alive
    kill b = b { alive = False }

createBall :: Vec2f -> Vec2f -> MaybeT IO Ball
createBall pos@(Vec2f x y) vel = do 
    liftIO $ putStrLn $ "Creating ball at " ++ show pos
    myCircle <- liftIO createCircleShape
    case myCircle of
        Left e -> liftIO (putStrLn $ "Error while creating a circle shape. " ++ show e) >> mzero
        Right r -> do
            let color = blue
            liftIO $ setFillColor r color
            liftIO $ setRadius r 25
            return (Ball r pos vel color True)