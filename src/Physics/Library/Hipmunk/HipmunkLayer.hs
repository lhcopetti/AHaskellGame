{-# LANGUAGE NamedFieldPuns #-}
module Physics.Library.Hipmunk.HipmunkLayer
    ( setLayers
    ) where

import qualified Physics.Hipmunk as H

import Data.StateVar
import Data.Word (Word32)

import Physics.Library.Hipmunk.PhysicsTypes

setLayers :: Word32 -> PhysicsObject -> IO ()
setLayers wrd = ($= wrd) . H.layers . shape