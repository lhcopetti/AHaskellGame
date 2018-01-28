module GameObject.GameObjectTypes
    ( GameObject (..)
    , BehaviorType
    , Behavior (..)
    , Creation
    , GameObjectCreation
    , CommandType
    , Command (..)
    , InputType
    , Input (..)
    , Size (..)
    , Ratio (..)
    , SpriteSheet (..)
    , Animation (..)
    , DrawingFlag (..)
    , Drawing (..)
    , DrawingMessageType
    , DrawingMessage (..)
    ) where


import SFML.Graphics.Types (CircleShape, RectangleShape, ConvexShape, Text, Sprite, Texture)
import SFML.System.Vector2 (Vec2f)

import Control.Monad.Reader (Reader)
import Control.Monad.Trans.Maybe (MaybeT)

import Component.Physics.Physics
import GameEnv
import Updatable (UpdateType)

data GameObject = GameObject { drawComp     :: Drawing
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

type BehaviorType = GameObject -> Reader GameEnvironment GameObject

type Creation a = MaybeT IO a
type GameObjectCreation = Creation GameObject

data Behavior = Behavior {  behave :: BehaviorType
                         }


type CommandType    = UpdateType GameObject
type InputType a    = GameObject -> Reader GameEnvironment a

data Command = Command CommandType

data Input = Input { runInput :: InputType GameObject
                   }

data Animation = Animation  { createDrawing :: Drawing -> Drawing
                            , interval      :: Float
                            , counter       :: Float
                            , spriteIndex   :: Int
                            , spriteSheet   :: SpriteSheet
                            , spriteLoop    :: [Int]
                            }

data Drawing    = CircleDrawing CircleShape
                | RectangleDrawing RectangleShape
                | ConvexDrawing ConvexShape
                | TextDrawing Text
                | SpriteDrawing Sprite Texture
                | CompositeDrawing [Drawing]
                | FlaggedDrawing Drawing [DrawingFlag]
                | NamedDrawing String Drawing
                | AnimationDrawing Animation Sprite

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