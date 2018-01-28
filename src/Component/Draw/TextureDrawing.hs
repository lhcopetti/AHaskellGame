module Component.Draw.TextureDrawing
    ( createTextureDrawing
    , getTextureSize
    ) where

import SFML.Graphics.Types (Texture)
import SFML.Graphics.Rect (IntRect)
import SFML.Graphics.Texture (textureFromFile, textureSize)
import SFML.System.Vector2 (Vec2u (..))

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Component.Draw.DrawingHelper (createShapeT)
import GameObject.GameObjectTypes (Size (..))


createTextureDrawing :: FilePath -> Maybe IntRect -> MaybeT IO Texture
createTextureDrawing path rect = do
    liftIO $ putStrLn $ "Loading Texture from file: " ++ path
    createShapeT (textureFromFile path rect)

getTextureSize :: Texture -> MaybeT IO Size
getTextureSize tex = do
    (Vec2u x y) <- liftIO (textureSize tex)
    return $ Size (fromIntegral x) (fromIntegral y)