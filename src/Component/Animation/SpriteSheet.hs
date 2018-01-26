module Component.Animation.SpriteSheet
    ( SpriteSheet (..)
    , Size
    , Ratio
    , getIntRect
    , loadSpriteSheet
    ) where

import SFML.Graphics.Types (Sprite, Texture)
import SFML.Graphics.Rect (IntRect (..))

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad (forM)

import Component.Draw.TextureDrawing (createTextureDrawing, getTextureSize)
import Component.Draw.SpriteDrawing (createSpriteTextureRect)

type Size   = (Int, Int)
type Ratio  = (Int, Int)

data SpriteSheet = SpriteSheet  { sprites   :: [Sprite]
                                , texture   :: Texture
                                , texSize   :: Size
                                , ratio     :: Ratio
                                }

loadSpriteSheet :: FilePath -> Ratio -> MaybeT IO SpriteSheet
loadSpriteSheet path ratio = do
    tex <- createTextureDrawing path Nothing
    textureSize <- getTextureSize tex
    sprites <- createSprites tex textureSize ratio
    return $ SpriteSheet sprites tex textureSize ratio

createSprites :: Texture -> Size -> Ratio -> MaybeT IO [Sprite]
createSprites tex (texWidth, texHeight) (countX, countY) = do
    let spriteSize = (texWidth `div` countX, texHeight `div` countY)
    let localIntRect = getIntRect spriteSize (countX, countY)
    forM [0..countX * countY -1] (createSpriteTextureRect tex . localIntRect)

getIntRect :: Size -> Ratio -> Int -> IntRect
getIntRect (w, h) (wRatio, hRatio) index = let
    (rectW, rectH) = (index `mod` wRatio, index `div` wRatio)
    in
        IntRect (rectW * w) (rectH * h) w h