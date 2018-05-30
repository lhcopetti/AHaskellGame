module Graphics.SFML.FontLoader
    ( loadFontFromMemory
    ) where

import Graphics.SFML.FromMemoryLoader (sfmlResourceFromMemory)

import SFML.Graphics.Font (fontFromMemory)
import SFML.Graphics.Types (Font)
import SFML.SFException

loadFontFromMemory :: String -> IO (Either SFException Font)
loadFontFromMemory = sfmlResourceFromMemory fontFromMemory