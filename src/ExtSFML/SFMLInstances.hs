{-# LANGUAGE StandaloneDeriving #-}
module ExtSFML.SFMLInstances
    () where

import SFML.Graphics.Rect (IntRect (..))
import SFML.System.Vector2 (Vec2f (..))
import SFML.Window.Keyboard (KeyCode (..))

deriving instance Show  IntRect
deriving instance Eq    IntRect

deriving instance Eq    Vec2f


deriving instance Ord   KeyCode