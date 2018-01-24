module PrefabObjects.TriangleMouseFollower
    ( followsAndDiesCloseToMouse
    , createMouseFollowerEqTriangle
    ) where

import SFML.System.Vector2
import SFML.Graphics.Color (yellow, white)

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createGameObject)
import GameObject.GameObjectTypes (Behavior (..), BehaviorType, GameObjectCreation, Creation, Command (..))
import GameObject.GameObject (addCommand)
import System.Messaging.DrawingMessage (DrawingMessage (..))
import Component.Draw.DrawingData (DrawingFlag (..))
import Component.Draw.Drawing (Drawing, setOriginDrawing)
import Component.Draw.NamedDrawing (createNamedDrawing)
import Component.Draw.CircleDrawing (createCenteredCircle)
import Component.Draw.TriangleDrawing (createEqTriangle)
import Component.Draw.CompositeDrawing (createComposite)
import Component.Draw.TextDrawing (createText)
import Component.Draw.FlaggedDrawing (createSingleFlagDrawing)
import Component.Behavior.MousePointerBehavior (followPointingMouse, mouseDistance)
import Component.Behavior.HigherOrderBehavior (behaviorPred, behaveOnceAndThen)
import Component.Behavior.TextBehavior (updateTextDrawing)
import Command.MessageCommand (sendDrwMsgCommand)
import Vec2.Vec2Math (zero)

import Command.ResetCommand (resetCommand)


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
    return (createGameObject drw (Behavior (followsAndDiesCloseToMouse 0)) zero zero)

createTextDrawing :: Creation Drawing
createTextDrawing = do
    text <- createText 15 "EqTriangle death counter"
    let namedText = createNamedDrawing "counter" text
    liftIO $ setOriginDrawing text (Vec2f 0 (-20))
    return $ createSingleFlagDrawing namedText NoRotationUpdates

followsAndDiesCloseToMouse :: Int -> BehaviorType
followsAndDiesCloseToMouse counter obj = do
    newObj <- followPointingMouse obj
    distanceToMouse <- mouseDistance newObj
    behaviorPred (distanceToMouse < 5.0) (resetAndAddScore counter) (followsAndDiesCloseToMouse counter) newObj

resetAndAddScore :: Int -> BehaviorType
resetAndAddScore score = do
    let newScore = score + 1
    behaveOnceAndThen (resetBehavior newScore) (followsAndDiesCloseToMouse newScore)

resetBehavior :: Int -> BehaviorType
resetBehavior counter obj = do
    let newObj = addCommand (Command resetCommand) obj
    let drawingMessage = updateTextDrawing ("Score: " ++ show counter)
    let namedMsg = NamedMessage "counter" drawingMessage
    let newObj' = addCommand (Command $ sendDrwMsgCommand namedMsg) newObj
    return newObj'