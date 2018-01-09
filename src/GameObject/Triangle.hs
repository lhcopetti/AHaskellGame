{-# LANGUAGE NamedFieldPuns #-}
module GameObject.Triangle
    ( Triangle (..)
    , createTriangle
    , update
    , draw
    ) where

import SFML.Graphics.Color
import SFML.Graphics.ConvexShape (createConvexShape, setPointCount, setPoint)
import SFML.System.Vector2
import Vec2.Vec2Math (zero, addVec2f)
import SFML.Graphics.Types
import SFML.Graphics.RenderWindow (drawConvexShape)
import SFML.Graphics.SFShape (setFillColor, setOutlineColor)
import SFML.Graphics.SFTransformable (setPosition)
import SFML.SFResource (destroy)

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
-- import qualified Component.Physics as Phy
import Behavior.BoxedBehavior (boundToDimension)

data Triangle = Triangle { pointer :: ConvexShape
                         , position :: Vec2f
                         , lifeCounter :: Int
                         }

instance Updatable Triangle where
    update = return . die

instance Synchronizable Triangle where
    synchronize t = setPosition (pointer t) (position t)

instance Drawable Triangle where 
    draw wnd Triangle { pointer } = drawConvexShape wnd pointer Nothing

instance Killable Triangle where
    isAlive = (> 0) . lifeCounter
    die t = t { lifeCounter = 0 }
    destroyResource = destroy . pointer

instance Pos.Position Triangle where
    getPosition = position
    setPosition triangle newPosition = triangle { position = newPosition }

createTriangle :: Vec2f -> Vec2f -> MaybeT IO Triangle
createTriangle pos@(Vec2f x y) vel = do 
    liftIO $ putStrLn $ "Creating triangle at " ++ show pos
    shape <- liftIO createConvexShape
    case shape of
        Left e -> do 
            liftIO (putStrLn $ "Error while trying to create a convex shape. " ++ show e)
            mzero
        Right p -> do
            let color = blue
            let initialLifeCounter = 5
            liftIO $ do 
                setPointCount p 3
                setPoint p 0 (Vec2f 55 30)
                setPoint p 1 (Vec2f 70 60)
                setPoint p 2 (Vec2f 40 60)
                setFillColor p color
                setOutlineColor p color
            return (Triangle p pos initialLifeCounter)