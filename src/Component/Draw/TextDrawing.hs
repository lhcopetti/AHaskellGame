module Component.Draw.TextDrawing
    ( createText
    , createEmptyText
    ) where

import qualified SFML.Graphics.Text as SFText (createText, setTextCharacterSize, setTextString, setTextFont)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Component.Draw.Drawing
import Component.Draw.DrawingHelper (createShapeT)
import Component.Draw.TextFont (loadFontT)

createEmptyText :: Int -> MaybeT IO Drawing
createEmptyText = (`createText` "")

createText :: Int -> String -> MaybeT IO Drawing
createText size text = do 
    liftIO $ putStrLn $ "Creating text Font-size: " ++ show size ++ " T: " ++ show text
    t <- createShapeT SFText.createText
    font <- loadFontT "resources/fonts/arial.ttf"
    liftIO $ do 
        SFText.setTextFont t font
        SFText.setTextString t text
        SFText.setTextCharacterSize t size
        return (TextDrawing t)