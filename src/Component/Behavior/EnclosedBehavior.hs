module Component.Behavior.EnclosedBehavior
    ( encloseToBox
    , encloseByWrapAround
    ) where

import Control.Monad.Reader (asks)

import Behavior.BoxedBehavior (boundToDimension, wrapAround)
import Component.Behavior.Behavior
import GameObject.GameObject ()
import GameEnv


encloseToBox :: BehaviorType
encloseToBox obj = do
        dimension <- asks gameArea
        return (boundToDimension obj dimension)

encloseByWrapAround :: BehaviorType
encloseByWrapAround obj = do
    dimension <- asks gameArea
    return (wrapAround obj dimension)