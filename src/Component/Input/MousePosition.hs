module Component.Input.MousePosition
    ( MousePositionState (..)
    , getMouseState
    ) where

import SFML.System.Vector2
import Control.Monad.Reader (asks)

import GameObject.GameObjectTypes
import GameEnv
import Input.Mouse

data MousePositionState = WithinBounds | OutOfBounds
    deriving (Eq)

getMouseState :: GoUpdateMStack MousePositionState
getMouseState = do
    pos <- asks (mousePos . input)
    bounds <- asks gameArea
    return (checkBoundary pos bounds)

checkBoundary :: Vec2f -> Vec2u -> MousePositionState
checkBoundary (Vec2f x y) (Vec2u w h)
        | x < 0 || y < 0    = OutOfBounds
        | x > w' || y > h'  = OutOfBounds
        | otherwise         = WithinBounds
        where
            h' = fromIntegral h
            w' = fromIntegral w
        