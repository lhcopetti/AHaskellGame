module Physics.PolygonPhysics
    ( mkPolygonPhysics
    , mkPolygonPhysicsD
    ) where
    
import SFML.System.Vector2 (Vec2f)

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.IO.Class (liftIO)

import qualified Physics.Library.Hipmunk.HipmunkPolygon as HMP
import GameObject.GameObjectTypes
import Physics.PhysicsTypes (PhysicsWorld)
import Physics.Library.Hipmunk.VectorConversion (vec2fToHVector)
import Physics.DebugDraw.DebugDraw (mkDebugDraw)

mkPolygonPhysics :: Vec2f -> [Vec2f] -> PhysicsWorld -> IO Physics
mkPolygonPhysics pos = HMP.mkPolygonPhysics (vec2fToHVector pos) . map vec2fToHVector
        
mkPolygonPhysicsD :: Vec2f -> [Vec2f] -> PhysicsWorld -> MaybeT IO (Physics, Drawing)
mkPolygonPhysicsD pos points world = do
    physics <- liftIO $ mkPolygonPhysics pos points world
    draw    <- mkDebugDraw physics
    return (physics, draw)