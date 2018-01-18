module Component.Behavior.Behaviors
    ( encloseToBoxB
    , encloseByWrapAroundB
    , noopB
    , deadManWalkingB
    , mousePointerB
    , mouseFollowerB
    , rotateB
    ) where

import Component.Behavior.Behavior
import Component.Behavior.EnclosedBehavior (encloseToBox, encloseByWrapAround)
import Component.Behavior.MousePointerBehavior (mousePointer, mouseFollower)
import Component.Behavior.RotationalBehavior (rotate)
import Killable (die)

encloseToBoxB :: Behavior
encloseToBoxB = Behavior encloseToBox

encloseByWrapAroundB :: Behavior
encloseByWrapAroundB = Behavior encloseByWrapAround

noopB :: Behavior 
noopB = Behavior return

deadManWalkingB :: Behavior
deadManWalkingB = Behavior (return . die)

mousePointerB :: Behavior 
mousePointerB = Behavior mousePointer

mouseFollowerB :: Behavior
mouseFollowerB = Behavior mouseFollower

rotateB :: Float -> Behavior
rotateB f = Behavior (rotate f)