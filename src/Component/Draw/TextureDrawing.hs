module Component.Draw.TextureDrawing
    ( createTextureDrawing
    , getTextureSize
    ) where

import SFML.Graphics.Types (Texture)
import SFML.Graphics.Rect (IntRect)
import SFML.Graphics.Texture (textureSize)
import SFML.System.Vector2 (Vec2u (..))

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)

import GameObject.GameObjectTypes (Size (..))
import Graphics.SFML.StaticLoader (loadTexture)


createTextureDrawing :: FilePath -> Maybe IntRect -> MaybeT IO Texture
createTextureDrawing path rect = do
    liftIO $ putStrLn $ "Loading Texture from file: " ++ path
    let resourceName = "sprites/" ++ path
    texture <- liftIO (loadTexture rect resourceName)
    case texture of
        Left e -> do
            liftIO $ putStrLn ("Error while loading texture: " ++ show e)
            mzero
        Right r -> return r

getTextureSize :: Texture -> MaybeT IO Size
getTextureSize tex = do
    (Vec2u x y) <- liftIO (textureSize tex)
    return $ Size (fromIntegral x) (fromIntegral y)