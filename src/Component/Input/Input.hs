module Component.Input.Input
    ( isPressed
    , emptyInput
    ) where

import SFML.Window.Keyboard (KeyCode (..))

import Control.Monad.Reader (asks)

import GameObject.GameObjectTypes
import GameEnv (GameEnvironment (..))
import System.InputSnapshot (InputSnapshot (..))
import Updatable (UpdateMStack)

emptyInput :: Input
emptyInput = Input return

isPressed :: KeyCode -> UpdateMStack Bool
isPressed key = do
    pressedKeys <- asks (pressed . inputSnapshot)
    return (key `elem` pressedKeys)