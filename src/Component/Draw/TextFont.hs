module Component.Draw.TextFont
    ( fontFromFileT
    , loadFontT
    ) where


import SFML.Graphics.Types (Font)
import SFML.Graphics.Font (fontFromFile)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)

import Resources.StaticResourceResolver (getFontPath)

loadFontT :: String -> MaybeT IO Font
loadFontT = fontFromFileT . getFontPath

fontFromFileT :: FilePath -> MaybeT IO Font
fontFromFileT filePath = do
    result <- liftIO $ fontFromFile filePath
    case result of
        Left e -> do
            liftIO $ putStrLn $ "Error while loading font from file: " ++ show e
            mzero
        Right f -> return f