{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Component.Animation.AnimationDrawing
    ( Animation (..)
    , createAnimation
    , updateAnimation
    , destroyAnimation
    ) where

import SFML.Graphics.Types (Sprite)

import GameObject.GameObjectTypes (Animation (..), Drawing (..))
import Component.Animation.SpriteSheet (SpriteSheet (..), spriteByIndex, destroySpriteSheet)

createAnimation :: SpriteSheet -> Float -> (Drawing -> Drawing) -> [Int] -> Drawing
createAnimation ss i create loop =
    createNewDrawing Animation 
        { createDrawing = create 
        , interval = i
        , counter = 0
        , spriteIndex = 0
        , spriteSheet = ss
        , spriteLoop = loop
        }

incrementCounter :: Float -> Animation -> Animation
incrementCounter f anim @ Animation { counter } = anim { counter = counter + f }

checkInterval :: Animation -> (Animation, Bool)
checkInterval anim @ Animation{..} = 
    if counter < interval then (anim, False) else (newAnim, True)
    where
        newIndex = (spriteIndex + 1) `mod` length spriteLoop
        newAnim = anim { counter = 0, spriteIndex = newIndex }

updateAnimation :: Animation -> Sprite -> Drawing
updateAnimation anim spr = let
    anim' = incrementCounter 1.0 anim
    (newAnim, res) = checkInterval anim'
    in if not res then AnimationDrawing newAnim spr
    else createNewDrawing newAnim

createNewDrawing :: Animation -> Drawing
createNewDrawing anim@Animation{..} = let
    newSprite = spriteByIndex (spriteLoop !! spriteIndex) spriteSheet
    in createDrawing (AnimationDrawing anim newSprite)

destroyAnimation :: Animation -> IO ()
destroyAnimation Animation{..} = destroySpriteSheet spriteSheet