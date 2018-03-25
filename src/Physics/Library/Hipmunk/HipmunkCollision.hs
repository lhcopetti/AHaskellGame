module Physics.Library.Hipmunk.HipmunkCollision
    ( collisionPoints
    , emptyCollisionData
    , appendPoints
    , getCollisionData
    ) where

import qualified Physics.Hipmunk as H

import Data.IORef (readIORef)

import Physics.Library.Hipmunk.PhysicsTypes

emptyCollisionData :: PhyCollisionData
emptyCollisionData = CD { points = [] 
                        }

collisionPoints :: PhyCollisionData -> [H.Position]
collisionPoints = points

appendPoints :: [H.Position] -> PhyCollisionData -> PhyCollisionData
appendPoints ps collData = collData { points = ps ++ points collData }

getCollisionData :: PhysicsWorld -> IO PhyCollisionData
getCollisionData = readIORef . collCallback