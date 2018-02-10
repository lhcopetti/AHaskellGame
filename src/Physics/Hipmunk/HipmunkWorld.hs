module Physics.Hipmunk.HipmunkWorld
    ( createSpace

    ) where

import qualified Physics.Hipmunk as H
import Data.StateVar

createSpace :: IO H.Space
createSpace = do
    space <- H.newSpace
    H.gravity space $= H.Vector 0 30 -- Default gravity
    return space