module HipmunkSimple where

import qualified Data.Map as M
import Control.Monad
import Data.IORef
import Data.StateVar as SV
import System.Exit

import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL as GL
import qualified Physics.Hipmunk as H

type Time = Double
    
------------------------------------------------------------
-- Some constants and utils
------------------------------------------------------------

-- | Desired (and maximum) frames per second.
desiredFPS :: Int
desiredFPS = 60

-- | How much seconds a frame lasts.
framePeriod :: Time
framePeriod = 1 / toEnum desiredFPS

-- | How many steps should be done per frame.
frameSteps :: Double
frameSteps = 6

-- | Maximum number of steps per frame (e.g. if lots of frames get
--   dropped because the window was minimized)
maxSteps :: Int
maxSteps = 20

-- | How much time should pass in each step.
frameDelta :: H.Time
frameDelta = 3.33e-3

-- | 0 :: GLfloat
zero :: GLfloat
zero = 0

-- | Asserts that an @IO@ action returns @True@, otherwise
--   fails with the given message.
assertTrue :: IO Bool -> String -> IO ()
assertTrue act msg = do {b <- act; when (not b) (fail msg)}

-- | Constructs a Vector.
(+:) :: H.CpFloat -> H.CpFloat -> H.Vector
(+:) = H.Vector
infix 4 +:



------------------------------------------------------------
-- State
------------------------------------------------------------

-- | Our current program state that will be passed around.
data State = State {
      stSpace    :: H.Space,
      stShapes   :: M.Map H.Shape (IO () {- Drawing -}
                                  ,IO () {- Removal -})
    }
type Object = (H.Shape, (IO (), IO ()))

-- | Our initial state.
initialState :: IO State
initialState = do
  -- The (empty) space
  space <- H.newSpace
  H.gravity space SV.$= 0 +: -30

  -- Default objects
  ground  <- buildGround space
  ball    <- createCircle' space 0 0
  ball1   <- createCircle' space (5 +: 200) 0
  ball'   <- createCircle' space (100 +: 0) 0
  ball''  <- createCircle' space (-100 +: 0) 0
  ball2   <- createCircle' space (0 +: 100) 0

  -- Add a callback to Hipmunk to draw collisions
  -- collisionsVar <- newIORef []
  -- let handler = do ps <- H.points
  --                  liftIO $ modifyIORef collisionsVar (ps ++)
  -- H.setDefaultCollisionHandler space
  --   H.Handler { H.beginHandler     = Nothing
  --             , H.preSolveHandler  = Nothing
  --             , H.postSolveHandler = Just handler
  --             , H.separateHandler  = Nothing}

  -- Our state
  return State { stSpace    = space
               , stShapes   = M.fromList [ground, ball, ball1, ball', ball'', ball2]
               }

-- | Builds the ground
buildGround :: H.Space -> IO Object
buildGround space = do
  static <- H.newBody H.infinity H.infinity
  H.position static SV.$= -330 +: 0
  let seg1type = H.LineSegment (50 +: -230) (610 +: -230) 1
  seg1   <- H.newShape static seg1type 0
  H.friction   seg1 SV.$= 1.0
  H.elasticity seg1 SV.$= 0.6
  H.spaceAdd space (H.Static seg1)
  return (seg1, (drawMyShape seg1 seg1type, return ()))

-- | Destroy a state.
destroyState :: State -> IO ()
destroyState (State {stSpace = space}) = do
  H.freeSpace space




------------------------------------------------------------
-- Main function and main loop
------------------------------------------------------------
initLibrary :: IO ()
initLibrary = do
  -- Initialize Chipmunk, GLFW and our state
  H.initChipmunk
  assertTrue initialize "Failed to init GLFW"
  stateVar <- initialState >>= newIORef

  -- Create a window
  assertTrue (openWindow (GL.Size 1200 900) [] Window) "Failed to open a window"
  windowTitle GL.$= "Hipmunk Playground"

  -- Define some GL parameters for the whole program
  clearColor  GL.$= Color4 1 1 1 1
  pointSmooth GL.$= Enabled
  pointSize   GL.$= 3
  lineSmooth  GL.$= Enabled
  lineWidth   GL.$= 2.5
  blend       GL.$= Enabled
  blendFunc   GL.$= (SrcAlpha, OneMinusSrcAlpha)
  matrixMode  GL.$= Projection
  loadIdentity
  ortho (-320) 320 (-240) 240 (-1) 1
  translate (Vector3 0.5 0.5 zero)

  -- Add some callbacks to GLFW
  windowCloseCallback GL.$= exitSuccess
  windowSizeCallback  GL.$= (\size -> viewport GL.$= (Position 0 0, size))

  -- Let's go!
  now <- GL.get time
  loop stateVar now

-- | The simulation loop.
loop :: IORef State -> Time -> IO ()
loop stateVar oldTime = do
  -- Some key states
  quitKey  <- getKey (SpecialKey ESC)
  -- Quit?
  when (quitKey == Press) (terminate >> exitSuccess)

  -- Update display and time
  updateDisplay stateVar
  newTime <- advanceTime stateVar oldTime
  loop stateVar newTime

------------------------------------------------------------
-- Display related functions
------------------------------------------------------------


-- | Renders the current state.
updateDisplay :: IORef State -> IO ()
updateDisplay stateVar = do
  state <- SV.get stateVar
  clear [ColorBuffer]
  forM_ (M.assocs $ stShapes state) (fst . snd) -- Draw each one
  swapBuffers

-- | Draws a shape (assuming zero offset)
drawMyShape :: H.Shape -> H.ShapeType -> IO ()
drawMyShape shape (H.Circle radius) = do
  H.Vector px py <- SV.get $ H.position $ H.body shape
  angle          <- SV.get $ H.angle    $ H.body shape

  color $ Color3 zero zero zero
  renderPrimitive LineStrip $ do
    let segs = 20; coef = 2*pi/toEnum segs
    forM_ [0..segs] $ \i -> do
      let r = toEnum i * coef
          x = radius * cos (r + angle) + px
          y = radius * sin (r + angle) + py
      vertex' x y
    vertex' px py
  drawPoint PositionPoint (px +: py)
drawMyShape shape (H.LineSegment p1 p2 _) = do
  let v (H.Vector x y) = vertex' x y
  pos <- SV.get $ H.position $ H.body shape
  color $ Color3 zero zero zero
  renderPrimitive Lines $ v (p1 + pos) >> v (p2 + pos)
  drawPoint PositionPoint pos
drawMyShape shape (H.Polygon verts) = do
  pos   <- SV.get $ H.position $ H.body shape
  angle <- SV.get $ H.angle    $ H.body shape
  let rot = H.rotate $ H.fromAngle angle
      verts' = map ((+pos) . rot) verts
  color $ Color3 zero zero zero
  renderPrimitive LineStrip $ do
    forM_ (verts' ++ [head verts']) $ \(H.Vector x y) -> do
      vertex' x y
  drawPoint PositionPoint pos

-- | Draws a red point.
drawPoint :: PointType -> H.Vector -> IO ()
drawPoint pt (H.Vector px py) = do
  color $ case pt of
            PositionPoint  -> Color3 zero zero 1
            CollisionPoint -> Color3 1 zero zero
  renderPrimitive Points $ do
    vertex' px py

data PointType = PositionPoint | CollisionPoint
                 deriving (Eq, Ord, Show, Enum)


createCircle' :: H.Space -> H.Position -> Double -> IO Object
createCircle' space pos angVel = do
  let mass   = 20
      radius = 20
      t = H.Circle radius
  b <- H.newBody mass $ H.momentForCircle mass (0, radius) 0
  s <- H.newShape b t 0
  ----
  H.position b SV.$= pos
  H.angVel     b SV.$= angVel
  H.friction   s SV.$= 0.5
  H.elasticity s SV.$= 0.9
--   let add space = do
  H.spaceAdd space b
  H.spaceAdd space s
  let draw = do
          drawMyShape s t
  let remove = do
          H.spaceRemove space b
          H.spaceRemove space s
  return (s, (draw, remove))


vertex' :: Double -> Double -> IO ()
vertex' x y = let f p = realToFrac p :: GLdouble
              in vertex (Vertex2 (f x) (f y))

------------------------------------------------------------
-- Simulation bookkeeping
------------------------------------------------------------

-- | Advances the time in a certain number of steps.
advanceSimulTime :: IORef State -> Int -> IO ()
advanceSimulTime _        0     = return ()
advanceSimulTime stateVar steps = do
  state <- SV.get stateVar

  let step = H.step (stSpace state) frameDelta
  replicateM_ (steps-1) step

  -- Do a final step that will leave the collisions variable filled.
  step

-- | Advances the time.
advanceTime :: IORef State -> Time -> IO Time
advanceTime stateVar oldTime = do
  newTime <- GL.get time
    -- Advance simulation
  let mult   = frameSteps / framePeriod
      framesPassed   = truncate $ mult * (newTime - oldTime)
      simulNewTime   = oldTime + toEnum framesPassed / mult
  advanceSimulTime stateVar $ min maxSteps framesPassed

  -- Correlate with reality
  newTime' <- GL.get time
  let diff = newTime' - simulNewTime
      sleepTime = framePeriod - diff
  when (sleepTime > 0) $ sleep sleepTime
  return simulNewTime
