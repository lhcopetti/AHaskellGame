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
import SFML.Graphics.SFTransformable (setPosition)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT)

import Updatable
import Synchronizable
import Drawable
import Killable
import qualified Component.Position as Comp
import Component.Draw.CircleDrawing (createCircle)
import Component.Draw.Drawing

data Dot = Dot { drawComp :: Drawing
               , position :: Vec2f
               , alive    :: Bool
               }

dotColor :: Color
dotColor = white

instance Updatable Dot where 
    update = return

instance Synchronizable Dot where
    synchronize dot = updateDrawing (drawComp dot) dot 

instance Drawable Dot where 
    draw wnd dot = draw wnd (drawComp dot)

instance Comp.Position Dot where
    getPosition = position
    setPosition d pos = d { position = pos }

instance Killable Dot where
    isAlive = alive
    die d = d { alive = False }
    destroyResource dot = destroyDrawing (drawComp dot)

createDot :: Vec2f -> MaybeT IO Dot
createDot pos@(Vec2f x y) = do 
    liftIO $ putStrLn $ "Creating dot at " ++ show pos
    myCircle <- createCircle 5 dotColor
    return (Dot myCircle pos True)