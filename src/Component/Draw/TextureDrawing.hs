module Component.Draw.TextureDrawing
    ( createTextureDrawing
    ) where

import SFML.System.Vector2 (Vec2f (..))
import SFML.Graphics.Types (Texture)
import SFML.Graphics.Rect (IntRect)
import SFML.Graphics.Texture (textureFromFile)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Component.Draw.Drawing
import Component.Draw.DrawingHelper (createShapeT)


createTextureDrawing :: FilePath -> Maybe IntRect -> MaybeT IO Texture
createTextureDrawing path rect = do
    liftIO $ putStrLn $ "Creating Texture from file: " ++ path
    createShapeT (textureFromFile path rect)