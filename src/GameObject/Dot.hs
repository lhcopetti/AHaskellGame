{-# LANGUAGE NamedFieldPuns #-}
module GameObject.Dot
    ( Dot (..)
    , createDot
    , draw
    ) where


import SFML.Graphics.Color
import SFML.Graphics.CircleShape
import SFML.System.Vector2
import SFML.Graphics.Types
import SFML.Graphics.RenderWindow (drawCircle)
import SFML.Graphics.SFShape (setFillColor)
import SFML.Graphics.SFTransformable (setPosition)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad (mzero)

import Updatable
import Synchronizable
import Drawable
import Killable
import qualified Component.Position as Comp

data Dot = Dot { pointer  :: CircleShape
               , position :: Vec2f
               , alive    :: Bool
               }

dotColor :: Color
dotColor = white

instance Updatable Dot where 
    update = return

instance Synchronizable Dot where
    synchronize x = return ()

instance Drawable Dot where 
    draw wnd Dot { pointer } = drawCircle wnd pointer Nothing


instance Comp.Position Dot where
    getPosition = position
    setPosition d pos = d { position = pos }

instance Killable Dot where
    isAlive = alive
    kill d = d { alive = False }

createDot :: Vec2f -> MaybeT IO Dot
createDot pos@(Vec2f x y) = do 
    liftIO $ putStrLn $ "Creating dot at " ++ show pos
    myCircle <- liftIO createCircleShape
    case myCircle of
        Left e -> do 
            liftIO (putStrLn $ "Error while trying to create a circle shape. " ++ show e)
            mzero
        Right r -> do
            liftIO $ setFillColor r dotColor
            liftIO $ setPosition r pos
            liftIO $ setRadius r 5
            return (Dot r pos True)