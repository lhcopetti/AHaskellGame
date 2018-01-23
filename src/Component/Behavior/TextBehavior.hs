{-# LANGUAGE RankNTypes #-}
module Component.Behavior.TextBehavior
    ( updatePromptForGOCount
    , updateTextDrawing
    , updateTextWithMousePosition
    , updateMultipleTexts
    ) where

import SFML.Graphics.Text (setTextString)
import SFML.System.Vector2 (Vec2f (..))

import Control.Monad.Reader (asks)

import GameObject.GameObject ()
import Component.Behavior.Behavior
import Component.Draw.DrawingData (Drawing (..))
import System.Messaging.DrawingMessage (DrawingMessageType, DrawingMessage (..), setInbox)
import GameEnv (GameEnvironment (..))
import Input.Mouse (mousePos)

updatePromptForGOCount :: String -> BehaviorType
updatePromptForGOCount prompt obj = do
    numberOfGameObjects <- asks countGOs
    let msg = MSG $ updateTextDrawing (prompt ++ ": " ++ show numberOfGameObjects)
    return (setInbox [msg] obj)

updateMultipleTexts :: Int -> BehaviorType
updateMultipleTexts count obj = do
    let fstMsg = NamedMessage "title" (updateTextDrawing $ "This is the title:" ++ show count)
    let sndMsg = NamedMessage "subtitle" (updateTextDrawing $ "This is the subtitle:" ++ show count)
    let newObj = setInbox [fstMsg, sndMsg] obj
    let newObj' = setBehaviorT (updateMultipleTexts (count + 1)) newObj
    return newObj'

updateTextWithMousePosition :: BehaviorType
updateTextWithMousePosition obj = do
    (Vec2f x y) <- asks (mousePos . input)
    let txt = "mouse: (" ++ show x ++ ", " ++ show y ++ ")"
    let msg = MSG $ updateTextDrawing txt
    return (setInbox [msg] obj)

updateTextDrawing :: String -> DrawingMessageType
updateTextDrawing text (TextDrawing ptr) = setTextString ptr text
updateTextDrawing _ _ = return ()