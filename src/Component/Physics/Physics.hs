{-# LANGUAGE NamedFieldPuns #-}
module Component.Physics.Physics
    ( Physics (..)
    , updatePosition
    ) where

import SFML.System.Vector2 (Vec2f (..))

import qualified Physics.Hipmunk as H

import Control.Monad (liftM)

import GameObject.GameObjectTypes
import GameObject.GameObject ()
import Vec2.Vec2Math (minVec2f, addVec2f)
import Component.Physics.PhysicsClass
import Physics.Hipmunk.VectorConversion (hVectorToVec2f)
import Component.Position
import Data.StateVar

instance PhysicsClass Physics where
    getVelocity (SimplePhy v _) = v
    getVelocity _ = Vec2f 0 0

    setVelocity (SimplePhy _ f) v' = SimplePhy (minVec2f v' f) f
    setVelocity p _ = p

    updatePhysics = return

instance PhysicsClass GameObject where
    getVelocity = getVelocity . physicsComp
    setVelocity go@GameObject { physicsComp } newVel = go { physicsComp = setVelocity physicsComp newVel }
    updatePhysics = updatePhysicsComponent

updatePosition :: (Position a, PhysicsClass a) => a -> a
updatePosition obj = let
    pos = getPosition obj
    vel = getVelocity obj
    newPos = addVec2f pos vel
    in setPosition obj newPos

updatePhysicsComponent :: GameObject -> IO GameObject
updatePhysicsComponent go = case physicsComp go of
    (SimplePhy _ _) -> updateSimplePhysics go
    _ -> updateHipPhy go

updateSimplePhysics :: GameObject -> IO GameObject
updateSimplePhysics = return . updatePosition

updateHipPhy :: GameObject -> IO GameObject
updateHipPhy go = case physicsComp go of
        (HipPhy b _ _) -> do position <- liftM hVectorToVec2f   . get . H.position  $ b
                             angle <-    liftM realToFrac       . get . H.angle     $ b
                             putStrLn $ "This is the position" ++ show position ++ show angle
                             liftM ((`setPosition` position) . setRotation angle) (return go)
        _ -> return go
