module PrefabObjects.TriangleMouseFollower
    ( followsAndDiesCloseToMouse
    , createMouseFollowerEqTriangle
    ) where

import SFML.System.Vector2
import SFML.Graphics.Color (yellow, white)

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createSimplePhysicsGO)
import GameObject.GameObjectTypes (Behavior (..), BehaviorType, GameObjectCreation, Creation, Command (..), DrawingFlag (..))
import GameObject.GameObject (addCommandM)
import System.Messaging.Messages.TextDrawingMessage (setTextMsg)
import System.Messaging.Handler.PushMessageHandler (pushNamedMessage)
import System.Messaging.Handler.RunMessageHandler (runMessageT)
import System.Messaging.Messages.TransformableMessage (setOriginMsg)
import Component.Draw.Drawing (Drawing)
import Component.Draw.NamedDrawing (createNamedDrawing)
import Component.Draw.CircleDrawing (createCenteredCircle)
import Component.Draw.TriangleDrawing (createEqTriangle)
import Component.Draw.CompositeDrawing (createComposite)
import Component.Draw.TextDrawing (createText)
import Component.Draw.FlaggedDrawing (createSingleFlagDrawing)
import Component.Behavior.MousePointerBehavior (followPointingMouse, mouseDistance)
import Component.Behavior.HigherOrderBehavior (behaviorPred, behaveOnceAndThen)
import Vec2.Vec2Math (zero)

import Command.ResetCommand (resetCommand)


createMouseFollowerEqTriangle :: GameObjectCreation st
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
        runMessageT (setOriginMsg (Vec2f triOriginX triOriginY1)) miniBall1
        runMessageT (setOriginMsg (Vec2f triOriginX triOriginY2)) miniBall2
    text <- createTextDrawing
    drw <- createComposite [drawComponent, miniBall1, miniBall2, text]
    return (createSimplePhysicsGO drw (Behavior (followsAndDiesCloseToMouse 0)) zero zero)

createTextDrawing :: Creation Drawing
createTextDrawing = do
    text <- createText 15 "EqTriangle death counter"
    let namedText = createNamedDrawing "counter" text
    liftIO $ runMessageT (setOriginMsg (Vec2f 0 (-20))) text
    return $ createSingleFlagDrawing namedText NoRotationUpdates

followsAndDiesCloseToMouse :: Int -> BehaviorType st
followsAndDiesCloseToMouse counter obj = do
    newObj <- followPointingMouse obj
    distanceToMouse <- mouseDistance newObj
    behaviorPred (distanceToMouse < 5.0) (resetAndAddScore counter) (followsAndDiesCloseToMouse counter) newObj

resetAndAddScore :: Int -> BehaviorType st
resetAndAddScore score = do
    let newScore = score + 1
    behaveOnceAndThen (resetBehavior newScore) (followsAndDiesCloseToMouse newScore)

resetBehavior :: Int -> BehaviorType st
resetBehavior counter obj = 
    addCommandM (Command resetCommand) obj >>=
    pushNamedMessage "counter" (setTextMsg ("Score: " ++ show counter))