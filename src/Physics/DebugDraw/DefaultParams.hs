module Physics.DebugDraw.DefaultParams
    ( lightColor
    , strongColor
    , lightThickness
    ) where

import SFML.Graphics.Color

import Data.Word (Word8)

defaultAlpha :: Word8
defaultAlpha = 50

lightColor :: Color
lightColor = green { a = defaultAlpha }

strongColor :: Color
strongColor = green

lightThickness :: Float
lightThickness = 1.0