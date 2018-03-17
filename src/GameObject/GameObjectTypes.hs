module GameObject.GameObjectTypes
    ( GameObject (..)
    , GoUpdateType
    , BehaviorType
    , Behavior (..)
    , Creation
    , GameObjectCreation
    , GameObjectsCreation
    , CommandType
    , Command (..)
    , InputType
    , Input (..)
    , Size (..)
    , Ratio (..)
    , SpriteSheet (..)
    , Animation (..)
    , DrawingFlag (..)
    , ZDrawing (..)
    , Drawing (..)
    , DrawingMessageType
    , DrawingMessage (..)
    , Physics (..)
    ) where


import SFML.Graphics.Types (CircleShape, RectangleShape, ConvexShape, Text, Sprite, Texture)
import Physics.PhysicsTypes
import SFML.System.Vector2 (Vec2f)

import Control.Monad.Trans.Maybe (MaybeT)
import qualified Data.List.NonEmpty as LNE

import Updatable (UpdateType)

data GameObject st = GameObject { drawComp     :: ZDrawing
                                , behavior     :: Behavior st
                                , physicsComp  :: Physics
                                , inputComp    :: Input st
                                , position     :: Vec2f
                                , rotation     :: Float
                                , inbox        :: [DrawingMessage]
                                , childObjects :: [GameObjectCreation st]
                                , commands     :: [Command st]
                                , alive        :: Bool
                                }

type GoUpdateType st = UpdateType (GameObject st) st
-- type GoUpdateMStack obj = UpdateMStack obj GoVoidState

-- type GameObjectST = GameObject GoVoidState

type BehaviorType st = GoUpdateType st

type Creation a = MaybeT IO a
type GameObjectCreation st  = Creation  (GameObject st)
type GameObjectsCreation st = Creation [GameObject st]

data Behavior st = Behavior {  behave :: BehaviorType st
                         }

-- data BehaviorState a = BehaviorState    { behaveState :: GameObject -> State a GameObject
--                                         }

type CommandType st    = GoUpdateType st
type InputType st    = GoUpdateType st

data Command st = Command (CommandType st)

data Input st = Input { runInput :: GoUpdateType st
                   }

data Animation = Animation  { createDrawing :: Drawing -> Drawing
                            , interval      :: Float
                            , counter       :: Float
                            , spriteIndex   :: Int
                            , spriteSheet   :: SpriteSheet
                            , spriteLoop    :: [Int]
                            }

data ZDrawing = ZDrawing Drawing Float

data Drawing    = CircleDrawing         CircleShape
                | RectangleDrawing      RectangleShape
                | ConvexDrawing         ConvexShape
                | TextDrawing           Text
                | SpriteDrawing         Sprite Texture
                | CompositeDrawing      (LNE.NonEmpty Drawing)
                | FlaggedDrawing        Drawing [DrawingFlag]
                | NamedDrawing          String Drawing
                | AnimationDrawing      Animation Sprite
                | PhysicsDebugDrawing   Drawing (IO ())
                | EmptyDrawing

data DrawingFlag 
    = NoRotationUpdates
    | NoPositionUpdates
        deriving (Eq)

data Size   = Size Int Int
data Ratio  = Ratio Int Int

data SpriteSheet = SpriteSheet  { sprites   :: [Sprite]
                                , texture   :: Texture
                                , texSize   :: Size
                                , ratio     :: Ratio
                                }

type DrawingMessageType = Drawing -> IO ()
data DrawingMessage = MSG DrawingMessageType
                    | NamedMessage String DrawingMessageType

data Physics = SimplePhy Vec2f Float
             | LibraryPhy PhyObject


-- updateWithState :: GameObject -> StateT a (Reader GameEnvironment) GameObject
-- updateWithState = undefined