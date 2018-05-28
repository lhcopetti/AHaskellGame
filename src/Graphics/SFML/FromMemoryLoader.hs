module Graphics.SFML.FromMemoryLoader
    ( sfmlResourceFromMemory
    ) where

import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.String (withCAStringLen)

import SFML.SFException


sfmlResourceFromMemory :: (Ptr b -> Int -> IO (Either SFException a)) 
                        -> String
                        -> IO (Either SFException a)
sfmlResourceFromMemory fromMemoryFunction strResource = 
    let fromMemory (ptr, len) = fromMemoryFunction (castPtr ptr) len
    in 
        withCAStringLen strResource fromMemory