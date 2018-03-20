module Component.Input.Input
    ( isMousePressed
    , isJustPressed
    , isKeyPressed
    , isKeyJustPressed
    , emptyInput
    , mousePosition
    ) where

import SFML.Window.Keyboard (KeyCode (..))
import SFML.System.Vector2 (Vec2f)

import Control.Monad.Reader (asks)

import GameObject.GameObjectTypes
import GameEnv (GameEnvironment (..))
import System.Input.InputSnapshot (InputSnapshot (..))
import Input.Mouse (mousePos)
import qualified System.Input.MouseSnapshot     as M (getButton, MButton)
import qualified System.Input.KeyboardSnapshot  as K (getKey)
import qualified System.Input.InputState        as I (isPressed, justPressed)
import Updatable

emptyInput :: Input st
emptyInput = Input return

isMousePressed :: M.MButton -> UpdateMStack Bool st
isMousePressed btn = asks (I.isPressed . M.getButton btn . mouse . inputSnapshot)

isJustPressed :: M.MButton -> UpdateMStack Bool st
isJustPressed btn = asks (I.justPressed . M.getButton btn . mouse . inputSnapshot)

isKeyPressed :: KeyCode -> UpdateMStack Bool st
isKeyPressed key = asks (I.isPressed . K.getKey key . keyboard . inputSnapshot)

isKeyJustPressed :: KeyCode -> UpdateMStack Bool st
isKeyJustPressed key = asks (I.justPressed . K.getKey key . keyboard . inputSnapshot)

mousePosition :: UpdateMStack Vec2f st
mousePosition  = asks (mousePos . input)