module Component.Behavior.Behaviors
    ( encloseToBoxB
    , encloseByWrapAroundB
    , noopB
    , deadManWalkingB
    ) where

import Component.Behavior.Behavior
import Component.Behavior.EnclosedBehavior (encloseToBox, encloseByWrapAround)
import Killable (die)

encloseToBoxB :: Behavior
encloseToBoxB = Behavior encloseToBox

encloseByWrapAroundB :: Behavior
encloseByWrapAroundB = Behavior encloseByWrapAround

noopB :: Behavior 
noopB = Behavior return

deadManWalkingB :: Behavior
deadManWalkingB = Behavior (return . die)