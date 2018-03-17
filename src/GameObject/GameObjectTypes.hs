module GameObject.GameObjectTypes
    ( GameObject (..)
    , GameObjectST
    , GoUpdateType
    , GoUpdateMStack
    , GoVoidState (..)
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

import Updatable (UpdateType, UpdateMStack)

data GameObject st = GameObject { drawComp     :: ZDrawing
                                , behavior     :: Behavior
                                , physicsComp  :: Physics
                                , inputComp    :: Input
                                , position     :: Vec2f
                                , rotation     :: Float
                                , inbox        :: [DrawingMessage]
                                , childObjects :: [GameObjectCreation]
                                , commands     :: [Command]
                                , alive        :: Bool
                                }

data GoVoidState = GVS

type GoUpdateType = UpdateType (GameObject GoVoidState) GoVoidState
type GoUpdateMStack obj = UpdateMStack obj GoVoidState

type GameObjectST = GameObject GoVoidState

type BehaviorType = GoUpdateType

type Creation a = MaybeT IO a
type GameObjectCreation  = Creation  (GameObject GoVoidState)
type GameObjectsCreation = Creation [GameObject GoVoidState]

data Behavior = Behavior {  behave :: BehaviorType
                         }

-- data BehaviorState a = BehaviorState    { behaveState :: GameObject -> State a GameObject
--                                         }

type CommandType    = UpdateType (GameObject GoVoidState) GoVoidState
type InputType a    = GoUpdateType

data Command = Command CommandType

data Input = Input { runInput :: GoUpdateType
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