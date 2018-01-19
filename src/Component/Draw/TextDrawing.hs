module Component.Draw.TextDrawing
    ( createText
    ) where

import SFML.Graphics.Types (Text)
import qualified SFML.Graphics.Text as SFText (createText, setTextCharacterSize, setTextString)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Component.Draw.Drawing
import Component.Draw.DrawingHelper (createShapeT)

createText :: Int -> String -> MaybeT IO Drawing
createText size text = do 
    liftIO $ putStrLn $ "Creating text Font-size: " ++ show size ++ "T: " ++ show text
    t <- createShapeT SFText.createText
    liftIO $ do 
        SFText.setTextString t text
        SFText.setTextCharacterSize t size 
        return (TextDrawing t)