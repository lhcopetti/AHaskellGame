module Physics.PhysicsLayer
    ( setLayers
    ) where

import Data.Word (Word32)

import Physics.PhysicsTypes (PhyObject)
import qualified Physics.Library.Hipmunk.HipmunkLayer as HMP (setLayers)


setLayers :: Word32 -> PhyObject -> IO ()
setLayers = HMP.setLayers