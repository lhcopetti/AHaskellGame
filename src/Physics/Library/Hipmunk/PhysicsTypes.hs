module Physics.Library.Hipmunk.PhysicsTypes
    ( PhysicsWorld (..)
    , PhysicsObject (..)
    , PhyCollisionData (..)
    ) where

import qualified Physics.Hipmunk as H

import Data.IORef (IORef)

data PhysicsWorld = PhysicsWorld    { space         :: H.Space
                                    , collCallback  :: IORef PhyCollisionData
                                    }

data PhysicsObject  = PL {  body        :: H.Body
                         ,  shape       :: H.Shape
                         ,  shapeType   :: H.ShapeType 
                         ,  delCallback :: IO ()
                         }

data PhyCollisionData = CD  { points :: [H.Position]
                            }