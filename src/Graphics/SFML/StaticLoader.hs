module Graphics.SFML.StaticLoader 
    ( loadFont
    , loadTexture
    ) where


import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)

import Resources.StaticResourceResolver (getResourceAsString)
import Graphics.SFML.FontLoader (loadFontFromMemory)
import Graphics.SFML.TextureLoader (loadTextureFromMemory)

import SFML.SFException (SFException (..))
import SFML.Graphics.Types (Font, Texture)
import SFML.Graphics.Rect (IntRect)

loadFont :: FilePath -> IO (Either SFException Font)
loadFont = resourceLoader loadFontFromMemory 

loadTexture :: Maybe IntRect -> FilePath -> IO (Either SFException Texture)
loadTexture rect = resourceLoader (loadTextureFromMemory rect)


resourceLoader ::   (String -> IO (Either SFException a))
                ->  FilePath
                ->  IO (Either SFException a)
resourceLoader function path = runExceptT $ do
    strContent <- liftEither (getResourceAsStringEither path)
    liftIO (function strContent) >>= liftEither


getResourceAsStringEither :: String -> Either SFException String
getResourceAsStringEither resource = 
    let result = getResourceAsString resource
    in case result of 
            Nothing -> Left (SFException $ "The resource named [" ++ resource ++ "] was not found")
            Just r  -> Right r