module Command.PositionCommand
    ( positionTopLeftCommand 
    , positionTopRightCommand
    , positionBottomLeftCommand
    , positionBottomRightCommand
    ) where

import SFML.System.Vector2 (Vec2f (..), Vec2u (..))

import Control.Monad.Reader (asks)

import Vec2.Vec2Math (zero)
import Component.Position (setPosition)
import GameObject.GameObjectTypes (CommandType)
import GameObject.GameObject ()
import GameEnv (gameArea)

positionTopLeftCommand :: CommandType
positionTopLeftCommand = return . (`setPosition` zero)

positionTopRightCommand :: CommandType
positionTopRightCommand obj = do
    (Vec2u width _) <- asks gameArea
    let topRight = Vec2f (fromIntegral width) 0
    return $ setPosition obj topRight

positionBottomLeftCommand :: CommandType
positionBottomLeftCommand obj = do
    (Vec2u _ height) <- asks gameArea
    let bottomLeft = Vec2f 0 (fromIntegral height)
    return $ setPosition obj bottomLeft

positionBottomRightCommand :: CommandType
positionBottomRightCommand obj = do
    (Vec2u width height) <- asks gameArea
    let bottomRight = Vec2f (fromIntegral width) (fromIntegral height)
    return $ setPosition obj bottomRight