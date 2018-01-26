{-# LANGUAGE StandaloneDeriving #-}
module ExtSFML.SFMLInstances
    () where

import SFML.Graphics.Rect (IntRect (..))

deriving instance Show  IntRect
deriving instance Eq    IntRect