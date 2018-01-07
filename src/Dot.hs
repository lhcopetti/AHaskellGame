{-# LANGUAGE InstanceSigs #-}
module Dot
    ( Dot
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

import Drawable

data Dot = Dot { circle   :: CircleShape
               , position :: Vec2f
               }

dotColor :: Color
dotColor = white

instance Drawable Dot where 
    draw :: RenderWindow -> Dot -> IO ()
    draw wnd (Dot circle pos) = drawCircle wnd circle Nothing

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
            return (Dot r pos)