{-# LANGUAGE StandaloneDeriving #-}
module ExtSFML.SFMLInstances
    () where

import SFML.Graphics.Rect (IntRect (..))
import SFML.System.Vector2 (Vec2f (..))

deriving instance Show  IntRect
deriving instance Eq    IntRect

deriving instance Eq    Vec2f