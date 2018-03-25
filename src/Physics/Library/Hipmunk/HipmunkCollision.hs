{-# LANGUAGE NamedFieldPuns #-}
module Physics.Library.Hipmunk.HipmunkCollision
    ( collisionPoints
    , emptyCollisionData
    , appendCollData
    , getCollisionData
    , hasCollided
    ) where

import qualified Physics.Hipmunk as H

import Data.IORef (readIORef)

import Physics.Library.Hipmunk.PhysicsTypes

emptyCollisionData :: PhyCollisionData
emptyCollisionData = CD { points = []
                        , shapes = []
                        }

collisionPoints :: PhyCollisionData -> [H.Position]
collisionPoints = points

appendCollData :: ((H.Shape, H.Shape), [H.Position]) -> PhyCollisionData -> PhyCollisionData
appendCollData (ss, ps) collData = collData 
    { points = ps ++ points collData 
    , shapes = ss : shapes collData
    }

getCollisionData :: PhysicsWorld -> IO PhyCollisionData
getCollisionData = readIORef . collCallback

hasCollided :: PhysicsObject -> PhyCollisionData -> Bool
hasCollided PL { shape } CD { shapes } = 
    let allShapes = concatMap (\(x, y) -> [x, y]) shapes
    in shape `elem` allShapes