{-# LANGUAGE ExistentialQuantification #-}

module GameObject.AnyGameObject
    ( AnyGameObject (..)
    , updateAnyGameObject
    , drawAnyGameObject
    ) where

        
import SFML.Graphics.Types (RenderWindow)

import Control.Monad.Trans.Reader (ReaderT)

import GameEnv (GameEnvironment)
import Drawable
import Updatable

data AnyGameObject = forall a. (Updatable a, Drawable a) => AGO a

updateAnyGameObject :: AnyGameObject -> ReaderT GameEnvironment IO AnyGameObject
updateAnyGameObject (AGO obj) = do 
    newObj <- update obj
    return (AGO newObj)

drawAnyGameObject :: RenderWindow -> AnyGameObject -> IO ()
drawAnyGameObject window (AGO obj) = draw window obj