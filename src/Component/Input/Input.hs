module Component.Input.Input
    ( isPressed
    , isMousePressed
    , isJustPressed
    , emptyInput
    ) where

import SFML.Window.Keyboard (KeyCode (..))

import Control.Monad.Reader (asks)

import GameObject.GameObjectTypes
import GameEnv (GameEnvironment (..))
import System.InputSnapshot (InputSnapshot (..))
import qualified System.MouseSnapshot as M (getButton, MButton)
import qualified System.InputState as I (isPressed, justPressed)
import Updatable

emptyInput :: Input st
emptyInput = Input return

isPressed :: KeyCode -> UpdateMStack Bool st
isPressed key = do
    pressedKeys <- asks (pressed . inputSnapshot)
    return (key `elem` pressedKeys)

isMousePressed :: M.MButton -> UpdateMStack Bool st
isMousePressed btn = asks (I.isPressed . M.getButton btn . mouse . inputSnapshot)

isJustPressed :: M.MButton -> UpdateMStack Bool st
isJustPressed btn = asks (I.justPressed . M.getButton btn . mouse . inputSnapshot)