{-# LANGUAGE RankNTypes #-}
module Component.Behavior.TextBehavior
    ( updatePromptForGOCount
    , updateTextDrawing
    ) where

import SFML.Graphics.Text (setTextString)

import Control.Monad.Reader (asks)

import Component.Behavior.Behavior
import Component.Draw.DrawingData (Drawing (..))
import System.Messaging.DrawingMessage (DrawingMessageType, DrawingMessage (..), setInbox)
import GameEnv (GameEnvironment (..))

updatePromptForGOCount :: String -> BehaviorType
updatePromptForGOCount prompt obj = do
    numberOfGameObjects <- asks countGOs
    let msg = MSG $ updateTextDrawing (prompt ++ ": " ++ show numberOfGameObjects)
    return (setInbox [msg] obj)


updateTextDrawing :: String -> DrawingMessageType
updateTextDrawing text (TextDrawing ptr) = setTextString ptr text
updateTextDrawing _ _ = return ()