module ObjectsFactory
    where

import SFML.System.Vector2
import SFML.Graphics.Color
import SFML.Window.Keyboard

import Control.Monad.IO.Class (liftIO)

import GameObjectFactory (createGameObject, createSimplePhysicsGO, createStaticGameObject, createStaticGameObjectB)
import GameObject.GameObjectTypes
import System.Messaging.Handler.RunMessageHandler (runMessageT)
import System.Messaging.Messages.TransformableMessage (setOriginMsg)
import Component.Draw.Drawing ()
import Component.Draw.CircleDrawing (createCircle, createCenteredCircle)
import Component.Draw.RectangleDrawing (createSquare)
import Component.Draw.ConvexDrawing (createConvex)
import Component.Draw.HexagonDrawing (createHexagon)
import Component.Draw.TextDrawing (createEmptyText, createText)
import Component.Draw.SpriteDrawing (createSpriteDrawing)
import Component.Draw.NamedDrawing (createNamedDrawing)
import Component.Draw.LineDrawing (createLine)
import Component.Draw.CompositeDrawing (createComposite)
import Component.Behavior.Behaviors
import Component.Behavior.CommandBehavior (addCommandBehavior)
import Component.Behavior.NoopBehavior (noopBehavior)
import Component.Input.Inputs (dieOnKeyPressing)
import Physics.CirclePhysics    (mkCirclePhysicsD)
import Physics.LinePhysics      (mkLinePhysicsD)
import Physics.PolygonPhysics   (mkPolygonPhysicsD)
import Physics.PhysicsTypes (PhysicsWorld)
import Command.PositionCommand
import Vec2.Vec2Math (zero)
import Math.Square (unitSquarePointsScaled)

createMiniBall :: Vec2f -> Vec2f -> GameObjectCreation
createMiniBall pos vel = do
    liftIO $ putStrLn $ "Creating mini ball at " ++ show pos
    let color = blue
    shape <- createCircle 2 color
    return (createSimplePhysicsGO shape encloseToBoxB pos vel)

createBall :: Vec2f -> Vec2f -> GameObjectCreation
createBall pos vel = do 
    liftIO $ putStrLn $ "Creating ball at " ++ show pos
    let color = blue
    shape <- createCircle 25 color
    return (createSimplePhysicsGO shape encloseToBoxB pos vel)

createRedBall :: Vec2f -> Vec2f -> GameObjectCreation
createRedBall pos vel = do 
    liftIO $ putStrLn $ "Creating red ball at " ++ show pos
    drawComponent <- createCircle 10 red
    return (createSimplePhysicsGO drawComponent encloseToBoxB pos vel)

createSquareObject :: Float -> Color -> Vec2f -> GameObjectCreation
createSquareObject side color pos = do
    liftIO $ putStrLn $ "Creating square at " ++ show pos
    draw <- createSquare side color
    return (createStaticGameObject draw pos) 

createYellowSquare :: Vec2f -> Vec2f -> GameObjectCreation
createYellowSquare pos vel = do
    liftIO $ putStrLn $ "Creating yellow square at " ++ show pos
    drawComponent <- createSquare 5 yellow
    return (createSimplePhysicsGO drawComponent encloseByWrapAroundB pos vel)

createMagentaWrapAroundBall :: Vec2f -> Vec2f -> GameObjectCreation
createMagentaWrapAroundBall pos vel = do
    liftIO $ putStrLn $ "Creating magenta ball at " ++ show pos
    drawComponent <- createCircle 5 magenta
    return (createSimplePhysicsGO drawComponent encloseByWrapAroundB pos vel)

createWhiteNoopBall :: Vec2f -> GameObjectCreation
createWhiteNoopBall pos = do
    liftIO $ putStrLn $ "Creating white noop ball " ++ show pos
    drawComponent <- createCircle 5 white
    return (createStaticGameObject drawComponent pos)

createDeadManWalking :: Vec2f -> GameObjectCreation
createDeadManWalking pos = do
    liftIO $ putStrLn $ "Creating dead man walking noop triangle " ++ show pos
    drawComponent <- createConvex white [Vec2f 55 30, Vec2f 70 60, Vec2f 40 60]
    return (createSimplePhysicsGO drawComponent deadManWalkingB pos zero)

createSimpleHexagon :: Vec2f -> GameObjectCreation
createSimpleHexagon pos = do
    liftIO $ putStrLn $ "Creating a simple hexagon " ++ show pos
    drawComponent <- createHexagon 25.0 white
    return (createSimplePhysicsGO drawComponent (rotateB 1.0) pos zero)

createMousePositionCopier :: GameObjectCreation
createMousePositionCopier = do
    liftIO $ putStrLn "Creating mouse pointer"
    drawComponent <- createCenteredCircle 3 green
    return (createSimplePhysicsGO drawComponent mousePositionCopierB zero zero)

createMouseFollower :: Vec2f -> GameObjectCreation
createMouseFollower pos = do
    liftIO $ putStrLn "Creating mouse follower"
    drawComponent <- createCenteredCircle 10 blue
    return (createSimplePhysicsGO drawComponent mouseFollowerB pos zero)

createSimpleText :: Vec2f -> String -> GameObjectCreation
createSimpleText pos text = do
    liftIO $ putStrLn "Creating simple text"
    drawComponent <- createText 30 text
    return (createStaticGameObject drawComponent pos)

createLiveGameObjectCounter :: Vec2f -> GameObjectCreation
createLiveGameObjectCounter pos = do
    liftIO $ putStrLn "Creating live GameObject counter"
    drawComponent <- createEmptyText 15
    let behavior = updatePromptForGOCountB "GameObjects"
    return (createStaticGameObjectB drawComponent pos behavior)

createDeathByUpdates :: Vec2f -> GameObjectCreation
createDeathByUpdates pos = do
    liftIO $ putStrLn "Creating object that dies from updates"
    drawComponent <- createCenteredCircle 10 blue
    let behavior = deathByUpdatesB
    return (createStaticGameObjectB drawComponent pos behavior)

createDeathByHitsOnWall :: Vec2f -> Vec2f -> GameObjectCreation
createDeathByHitsOnWall pos vel = do
    liftIO $ putStrLn "Creating object that dies from hitting on walls"
    drawComponent <- createCenteredCircle 15 green
    let behavior = deathByHitsOnWallB
    return (createSimplePhysicsGO drawComponent behavior pos vel)

createMousePositionPrinter :: Vec2f -> GameObjectCreation
createMousePositionPrinter pos = do
    liftIO $ putStrLn "Creating object that prints mouse position"
    drawComponent <- createEmptyText 10
    let behavior = updateTextWithMousePositionB
    return (createSimplePhysicsGO drawComponent behavior pos zero)

createSprite :: FilePath -> Vec2f -> Vec2f -> GameObjectCreation
createSprite path pos vel = do
    liftIO $ putStrLn ("Creating a Sprite GameObject from " ++ path)
    drawComponent <- createSpriteDrawing path
    let behavior = encloseByWrapAroundB
    return (createSimplePhysicsGO drawComponent behavior pos vel)

createBehaveOnce :: Vec2f -> GameObjectCreation
createBehaveOnce pos = do
    liftIO $ putStrLn "Creating object that behaves once"
    drawComponent <- createCenteredCircle 5 white
    let behavior = behaveOnceB (addChildB $ createWhiteNoopBall (Vec2f 600 230))
    return (createSimplePhysicsGO drawComponent behavior pos zero )

createNamedMessagesDemo :: Vec2f -> GameObjectCreation
createNamedMessagesDemo pos = do 
    liftIO $ putStrLn "Creating object that sends named messages"
    circle <- createCenteredCircle 5 white
    title <- createText 15 "this is it!"
    subtitle <- createText 15 "that was it!"
    allTogether <- createComposite [circle, createNamedDrawing "title" title, createNamedDrawing "subtitle" subtitle]
    liftIO $ do
        runMessageT (setOriginMsg (Vec2f 0 15)) title
        runMessageT (setOriginMsg (Vec2f 0 30)) subtitle
    let behavior = updateMultipleTextsB
    return (createSimplePhysicsGO allTogether behavior pos zero )

createUsesBehaveAll :: GameObjectCreation
createUsesBehaveAll = do
    liftIO $ putStrLn "Creates object that blinks in the four regions of the screen"
    drw <- createCenteredCircle 15 green
    let commands = map (addCommandBehavior . Command)   [ positionTopLeftCommand
                                                        , positionTopRightCommand
                                                        , positionBottomRightCommand
                                                        , positionBottomLeftCommand
                                                        ]
    let breaks = replicate (length commands) . replicate 50 $ noopBehavior
    let behaviors = concat $ zipWith (:) commands breaks
    let allTogether = behaveAllB (cycle behaviors)
    return (createSimplePhysicsGO drw allTogether (Vec2f 200 200) zero)

createHipPhysicsBall :: Vec2f -> Float -> PhysicsWorld -> GameObjectCreation
createHipPhysicsBall pos radius space = do
    liftIO $ putStrLn "Creating Hipmunk physics ball"
    (physics, drw) <- mkCirclePhysicsD radius pos space
    return (createGameObject drw encloseByWrapAroundB physics pos)

createPhysicsLine :: Float -> (Vec2f, Vec2f) -> PhysicsWorld -> GameObjectCreation
createPhysicsLine thickness line space = do
    liftIO $ putStrLn $ "Creating Hipmunk physics line at: " ++ show line
    (physics, draw) <- mkLinePhysicsD line thickness space
    let input = Input (dieOnKeyPressing KeyW)
        obj = createGameObject draw noopB physics (Vec2f 0 0)
    return (obj { inputComp = input })

createLine' :: (Vec2f, Vec2f) -> Float -> GameObjectCreation
createLine' line thickness = do
    liftIO $ putStrLn $ "Creating a line at: " ++ show line ++ " with T: " ++ show thickness
    let color = white
    drw <- createLine line thickness color
    return (createStaticGameObject drw (Vec2f 0 0))

createLines :: [(Vec2f, Vec2f, Float)] -> GameObjectsCreation
createLines = mapM (\(s, e, t) -> createLine' (s, e) t)

createBox :: Vec2f -> Float -> PhysicsWorld -> GameObjectCreation
createBox pos size world = do
    liftIO $ putStrLn $ "Creating box S: " ++ show size ++ " at " ++ show pos
    (physics, draw) <- mkPolygonPhysicsD pos (unitSquarePointsScaled size) world
    return (createGameObject draw noopB physics pos)