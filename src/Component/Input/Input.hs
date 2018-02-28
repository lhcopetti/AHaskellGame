module Component.Input.Input
    ( isPressed
    , emptyInput
    ) where

import SFML.Window.Keyboard (KeyCode (..))

import Control.Monad.Reader (Reader, asks)
import Control.Monad.Trans.State (StateT)

import GameObject.GameObjectTypes
import GameEnv (GameEnvironment (..))
import System.InputSnapshot (InputSnapshot (..))

emptyInput :: Input
emptyInput = Input return

isPressed :: KeyCode -> StateT Int (Reader GameEnvironment) Bool
isPressed key = do
    pressedKeys <- asks (pressed . inputSnapshot)
    return (key `elem` pressedKeys)