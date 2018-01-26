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
    , Animation (..)
    ) where


import SFML.System.Vector2 (Vec2f)
import System.Messaging.DrawingMessage

import Control.Monad.Reader (Reader)
import Control.Monad.Trans.Maybe (MaybeT)

import Component.Draw.Drawing
import Component.Animation.SpriteSheet (SpriteSheet)
import Component.Physics.Physics
import GameEnv
import Updatable (UpdateType)

data GameObject = GameObject { drawComp     :: Drawing
                             , behavior     :: Behavior
                             , physicsComp  :: Physics
                             , inputComp    :: Input
                             , animationComp:: Maybe Animation
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