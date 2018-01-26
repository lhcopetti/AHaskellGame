module Component.Draw.SpriteDrawing
    ( createSpriteTexture
    , createSpriteDrawing
    , createSpriteTextureRect
    ) where

import SFML.Graphics.Sprite (createSprite, setTexture, setTextureRect)
import SFML.Graphics.Types (Texture, Sprite)
import SFML.Graphics.Rect (IntRect)

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

createSpriteTextureRect :: Texture -> IntRect -> MaybeT IO Sprite
createSpriteTextureRect tex rect = do
    sprite <- createSpriteTexture tex
    liftIO $ setTextureRect sprite rect
    return sprite