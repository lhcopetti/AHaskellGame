module Component.Input.Input
    ( isPressed
    , isLeftMousePressed
    , isLeftJustPressed
    , emptyInput
    ) where

import SFML.Window.Keyboard (KeyCode (..))

import Control.Monad.Reader (asks)

import GameObject.GameObjectTypes
import GameEnv (GameEnvironment (..))
import System.InputSnapshot (InputSnapshot (..))
import System.MouseSnapshot (leftIsPressed, leftJustPressed)
import Updatable

emptyInput :: Input st
emptyInput = Input return

isPressed :: KeyCode -> UpdateMStack Bool st
isPressed key = do
    pressedKeys <- asks (pressed . inputSnapshot)
    return (key `elem` pressedKeys)

isLeftMousePressed :: UpdateMStack Bool st
isLeftMousePressed = asks (leftIsPressed . mouse . inputSnapshot)

isLeftJustPressed :: UpdateMStack Bool st
isLeftJustPressed = asks (leftJustPressed . mouse . inputSnapshot)