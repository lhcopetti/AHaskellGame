module Physics.Library.Hipmunk.PhysicsTypes
    ( PhysicsWorld (..)
    , PhysicsObject (..)
    ) where

import qualified Physics.Hipmunk as H

data PhysicsWorld = PhysicsWorld H.Space

data PhysicsObject  = PL {  body        :: H.Body
                         ,  shape       :: H.Shape
                         ,  shapeType   :: H.ShapeType 
                         ,  delCallback :: IO ()
                         }