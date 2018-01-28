{-# LANGUAGE RecordWildCards #-}
module Component.Animation.SpriteSheet
    ( SpriteSheet (..)
    , Size
    , Ratio
    , getIntRect
    , loadSpriteSheet
    , spriteCount
    , spriteByIndex
    , setScaleSpriteSheet
    , destroySpriteSheet
    ) where

import SFML.Graphics.Types (Sprite, Texture)
import SFML.Graphics.Rect (IntRect (..))
import SFML.System.Vector2 (Vec2f)
import SFML.SFResource (destroy)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad (forM, forM_)

import GameObject.GameObjectTypes (SpriteSheet (..), Size, Ratio)
import Component.Draw.TextureDrawing (createTextureDrawing, getTextureSize)
import Component.Draw.SpriteDrawing (setScaleSprite, createSpriteTextureRect)

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
getIntRect (w, h) (wRatio, _) index = let
    (rectW, rectH) = (index `mod` wRatio, index `div` wRatio)
    in
        IntRect (rectW * w) (rectH * h) w h

spriteCount :: SpriteSheet -> Int
spriteCount = length . sprites

spriteByIndex :: Int -> SpriteSheet -> Sprite
spriteByIndex index spriteSheet = sps !! (index `mod` spriteCount spriteSheet)
        where
            sps = sprites spriteSheet

setScaleSpriteSheet :: SpriteSheet -> Vec2f -> IO ()
setScaleSpriteSheet spr scale = forM_ (sprites spr) (setScaleSprite scale)

destroySpriteSheet :: SpriteSheet -> IO ()
destroySpriteSheet SpriteSheet {..} = mapM_ destroy sprites >> destroy texture