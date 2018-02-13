{-# LANGUAGE ExistentialQuantification #-}

module GameObject.AnyGameObject
    ( AnyGameObject (..)
    ) where

import Control.Monad (liftM)

import GameObject.GameObject ()
import Drawable
import Updatable
import Synchronizable
import Killable
import ChildBearer
import Component.Physics.PhysicsClass
import Component.Physics.Physics ()

data AnyGameObject = forall a. ( Updatable a
                               , Drawable a
                               , Synchronizable a
                               , Killable a
                               , ChildBearer a
                               , PhysicsClass a)
                               => AGO a

instance Updatable AnyGameObject where
    update (AGO go) = liftM AGO (update go)

instance Synchronizable AnyGameObject where
    synchronize (AGO go) = synchronize go

instance Drawable AnyGameObject where
    draw window (AGO go) = draw window go

instance Killable AnyGameObject where
    isAlive         (AGO go) = isAlive go
    die             (AGO go) = AGO (die go)
    destroyResource (AGO go) = destroyResource go

instance ChildBearer AnyGameObject where
    getChildren     (AGO go) = getChildren go
    removeChildren  (AGO go) = AGO (removeChildren go)
    addChild child  (AGO go) = AGO (addChild child go)

instance PhysicsClass AnyGameObject where
    getVelocity   (AGO go)   = getVelocity go
    setVelocity   (AGO go) v = AGO (setVelocity go v)
    updatePhysics (AGO go)   = liftM AGO (updatePhysics go)