module GameObject.GameObjectTypes
    ( GameObject (..)
    , BehaviorType
    , Behavior (..)
    ) where


import SFML.System.Vector2 (Vec2f)
import System.Messaging.DrawingMessage

import Control.Monad.Reader (Reader)

import Component.Draw.Drawing
import Component.Physics.Physics
import GameEnv

data GameObject = GameObject { drawComp     :: Drawing
                             , behavior     :: Behavior
                             , physicsComp  :: Physics
                             , position     :: Vec2f
                             , rotation     :: Float
                             , inbox        :: [DrawingMessage]
                             , alive        :: Bool
                             }

type BehaviorType = GameObject -> Reader GameEnvironment GameObject

data Behavior = Behavior {  behave :: BehaviorType
                         }

