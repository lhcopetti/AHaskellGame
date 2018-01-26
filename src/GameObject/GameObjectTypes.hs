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
    ) where


import SFML.System.Vector2 (Vec2f)
import System.Messaging.DrawingMessage

import Control.Monad.Reader (Reader)
import Control.Monad.Trans.Maybe (MaybeT)

import Component.Draw.Drawing
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