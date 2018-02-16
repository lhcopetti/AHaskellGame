{-# LANGUAGE NamedFieldPuns #-}
module Physics.Library.Hipmunk.DestroyObject
    ( destroyPhysics
    ) where

import Physics.Library.Hipmunk.PhysicsTypes

destroyPhysics :: PhysicsObject -> IO ()
destroyPhysics PL { delCallback } = delCallback