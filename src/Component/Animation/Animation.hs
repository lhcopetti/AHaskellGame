{-# LANGUAGE NamedFieldPuns #-}
module Component.Animation.Animation
    ( Animation (..)
    , createAnimation
    , updateAnimation
    , createNewDrawing
    ) where

import GameObject.GameObjectTypes (GameObject (..), Animation (..))
import Component.Animation.SpriteSheet (SpriteSheet (..), spriteByIndex)
import Component.Draw.DrawingData (Drawing (..))

createAnimation :: SpriteSheet -> Float -> (Drawing -> Drawing) -> [Int] -> Animation
createAnimation ss i create loop = Animation { createDrawing = create 
                                        , interval = i
                                        , counter = 0
                                        , spriteIndex = 0
                                        , spriteSheet = ss
                                        , spriteLoop = loop
                                        }



incrementCounter :: Float -> Animation -> Animation
incrementCounter f anim @ Animation { counter } = anim { counter = counter + f }

checkInterval :: Animation -> (Animation, Bool)
checkInterval anim @ Animation { counter , interval, spriteIndex } = 
    if counter < interval then (anim, False) else (newAnim, True)
    where
        newAnim = anim { counter = 0, spriteIndex = (spriteIndex + 1) `mod` length (spriteLoop anim) }

updateAnimation :: GameObject -> GameObject
updateAnimation go = case animationComp go of
    Nothing -> go
    Just anim -> runAnimation anim go

runAnimation :: Animation -> GameObject -> GameObject
runAnimation anim go = let
    anim' = incrementCounter 1.0 anim
    (newAnim, res) = checkInterval anim'
    in if not res then go { animationComp = Just newAnim }
    else let
        newDrawing = createNewDrawing newAnim
        in go { animationComp = Just newAnim, drawComp = newDrawing }

createNewDrawing :: Animation -> Drawing
createNewDrawing Animation { createDrawing, spriteSheet, spriteIndex, spriteLoop } = let
    newSprite = spriteByIndex (spriteLoop !! spriteIndex) spriteSheet
    in createDrawing (AnimationDrawing newSprite)