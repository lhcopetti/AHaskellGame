module Component.Behavior.EnclosedBehavior
    ( encloseToBox
    , encloseByWrapAround
    ) where

import Control.Monad.Reader (asks)

import Behavior.BoxedBehavior (boundToDimension, wrapAround)
import Component.Behavior.Behavior
import Component.Physics.Physics ()
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