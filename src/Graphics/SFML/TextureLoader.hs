module Graphics.SFML.TextureLoader
    ( loadTextureFromMemory
    ) where

import Foreign.Ptr (Ptr)

import Graphics.SFML.FromMemoryLoader (sfmlResourceFromMemory)

import SFML.Graphics.Rect (IntRect)
import SFML.Graphics.Texture (textureFromMemory)
import SFML.Graphics.Types (Texture)
import SFML.SFException


loadTextureFromMemory :: Maybe IntRect -> String -> IO (Either SFException Texture)
loadTextureFromMemory rect = sfmlResourceFromMemory (textureFromMemoryFlipped rect)


textureFromMemoryFlipped :: Maybe IntRect -> Ptr a -> Int -> IO (Either SFException Texture)
textureFromMemoryFlipped rect ptr len = textureFromMemory ptr len rect