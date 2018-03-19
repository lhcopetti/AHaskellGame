module Physics.PhysicsObject
    ( updateObjectPhysics
    ) where

import qualified Physics.Library.Hipmunk.HipmunkObject as HMP -- Hipmunk Physics
import Physics.PhysicsTypes
import GameObject.GameObjectTypes

import Component.Position
import System.Messaging.PhysicsInbox

updateObjectPhysics :: (Position a, PhysicsInbox a) => PhyObject -> a -> IO a
updateObjectPhysics phyObj obj = executeMessages phyObj obj >>= HMP.updateObjectPhysics phyObj

executeMessages :: (Position a, PhysicsInbox a) => PhyObject -> a -> IO a
executeMessages phyObj obj = do
    let msgs = getInbox obj
    mapM_ (`runMessage` phyObj) msgs
    return (clearInbox obj)

runMessage :: PhysicsMessage -> PhyObject -> IO ()
runMessage (PMSG f) = f