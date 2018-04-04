module Component.Behavior.EnclosedBehavior
    ( encloseToBox
    , encloseByWrapAround
    , behaveOutOfBounds
    ) where

import SFML.System.Vector2 (Vec2f (..), Vec2u (..))

import Control.Monad.Reader (asks)

import Behavior.BoxedBehavior (boundToDimension, wrapAround)
import Component.Behavior.Behavior
import Component.Physics.Physics ()
import Component.Position (getPosition)
import GameObject.GameObject ()
import GameEnv


encloseToBox :: BehaviorType st
encloseToBox obj = do
        dimension <- asks gameArea
        return (boundToDimension obj dimension)

encloseByWrapAround :: BehaviorType st
encloseByWrapAround obj = do
    dimension <- asks gameArea
    return (wrapAround obj dimension)

behaveOutOfBounds :: BehaviorType st -> BehaviorType st
behaveOutOfBounds beh obj = do
    let goPos = getPosition obj
    bounds <- asks gameArea
    if outOfBounds' goPos bounds then beh obj
    else return obj


outOfBounds' :: Vec2f -> Vec2u -> Bool
outOfBounds' pos (Vec2u w h) = outOfBounds pos (Vec2f w' h')
    where
        w' = realToFrac w
        h' = realToFrac h

outOfBounds :: Vec2f -> Vec2f -> Bool
outOfBounds (Vec2f px py) (Vec2f w h) = or
    [ px < 0
    , py < 0
    , px > w
    , py > h
    ]