module PrefabObjects.TriangleMouseFollower
    ( followsAndDiesCloseToMouse
    , createMouseFollowerEqTriangle
    ) where

import SFML.System.Vector2
import SFML.Graphics.Color (yellow, white)

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createGameObject)
import GameObject.GameObjectTypes (Behavior (..), BehaviorType, GameObjectCreation, Creation)
import Component.Draw.DrawingData (DrawingFlag (..))
import Component.Draw.Drawing (Drawing, setOriginDrawing)
import Component.Draw.CircleDrawing (createCircle, createCenteredCircle)
import Component.Draw.TriangleDrawing (createEqTriangle)
import Component.Draw.CompositeDrawing (createComposite)
import Component.Draw.TextDrawing (createText)
import Component.Draw.FlaggedDrawing (createSingleFlagDrawing)
import Component.Behavior.MousePointerBehavior (followPointingMouse, mouseDistance)
import Component.Behavior.DeathBehavior (dieBehavior)
import Component.Behavior.HigherOrderBehavior (behaviorPred, behaveBoth)
import Component.Behavior.ChildBearerBehavior (addChildBehavior)
import Vec2.Vec2Math (zero)


createMouseFollowerEqTriangle :: GameObjectCreation
createMouseFollowerEqTriangle = do
    liftIO $ putStrLn "Creating a follower eq triangle"
    let factor = 25.0
    drawComponent <- createEqTriangle factor white
    miniBall1 <- createCenteredCircle 5 yellow
    miniBall2 <- createCenteredCircle 5 yellow
    let triOriginX = sqrt 3 / 4 * factor + 5
    let triOriginY1 = 1/2 * factor + 5
    let triOriginY2 = -1/2 * factor + 5
    liftIO $ do
        setOriginDrawing miniBall1 (Vec2f triOriginX triOriginY1)
        setOriginDrawing miniBall2 (Vec2f triOriginX triOriginY2)
    text <- createTextDrawing
    drw <- createComposite [drawComponent, miniBall1, miniBall2, text]
    return (createGameObject drw (Behavior followsAndDiesCloseToMouse) zero zero)

createTextDrawing :: Creation Drawing
createTextDrawing = do
    text <- createText 15 "EqTriangle death counter"
    liftIO $ setOriginDrawing text (Vec2f 0 (-20))
    return $ createSingleFlagDrawing text NoRotationUpdates

followsAndDiesCloseToMouse :: BehaviorType
followsAndDiesCloseToMouse obj = do
    newObj <- followPointingMouse obj
    distanceToMouse <- mouseDistance newObj
    behaviorPred (distanceToMouse < 5.0) giveBirthBeforeDying followsAndDiesCloseToMouse newObj

giveBirthBeforeDying :: BehaviorType
giveBirthBeforeDying = let 
    fstBeh = addChildBehavior createMouseFollowerEqTriangle
    sndBeh = dieBehavior
    in
        behaveBoth fstBeh sndBeh