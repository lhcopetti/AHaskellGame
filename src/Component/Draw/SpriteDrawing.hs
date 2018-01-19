module Component.Draw.SpriteDrawing
    ( createSpriteTexture
    , createSpriteDrawing
    ) where

import SFML.System.Vector2 (Vec2f (..))
import SFML.Graphics.Sprite (createSprite, setTexture)
import SFML.Graphics.Types (Texture, Sprite)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import Component.Draw.Drawing
import Component.Draw.DrawingHelper (createShapeT)
import Component.Draw.TextureDrawing (createTextureDrawing)

createSpriteDrawing :: FilePath -> MaybeT IO Drawing
createSpriteDrawing path = do
    texture <- createTextureDrawing path Nothing
    sprite <- createSpriteTexture texture
    return (SpriteDrawing sprite texture)

createSpriteTexture :: Texture -> MaybeT IO Sprite
createSpriteTexture tex = do
    sprite <- createShapeT createSprite
    liftIO $ setTexture sprite tex True
    return sprite