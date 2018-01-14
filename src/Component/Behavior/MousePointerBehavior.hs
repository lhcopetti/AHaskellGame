module Component.Behavior.MousePointerBehavior
    ( mousePointer
    ) where

import Control.Monad.Reader (asks)

import Component.Behavior.Behavior (BehaviorType)
import GameEnv (GameEnvironment (..))

import Component.Position (setPosition)
import Input.Mouse (MouseInput (..))

mousePointer :: BehaviorType
mousePointer obj = do
    mousePosition <- asks (position . input)
    return (setPosition obj mousePosition)