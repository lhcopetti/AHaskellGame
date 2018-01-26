{-# LANGUAGE RankNTypes #-}
module Component.Behavior.TextBehavior
    ( updatePromptForGOCount
    , updateTextWithMousePosition
    , updateMultipleTexts
    ) where

import SFML.System.Vector2 (Vec2f (..))

import Control.Monad.Reader (asks)

import GameObject.GameObject ()
import Component.Behavior.Behavior
import System.Messaging.DrawingMessage (DrawingMessage (..), setInbox)
import GameEnv (GameEnvironment (..))
import Input.Mouse (mousePos)
import System.Messaging.TextDrawingMessage (setTextMsg)

updatePromptForGOCount :: String -> BehaviorType
updatePromptForGOCount prompt obj = do
    numberOfGameObjects <- asks countGOs
    let msg = MSG $ setTextMsg (prompt ++ ": " ++ show numberOfGameObjects)
    return (setInbox [msg] obj)

updateMultipleTexts :: Int -> BehaviorType
updateMultipleTexts count obj = do
    let fstMsg = NamedMessage "title" (setTextMsg $ "This is the title:" ++ show count)
    let sndMsg = NamedMessage "subtitle" (setTextMsg $ "This is the subtitle:" ++ show count)
    let newObj = setInbox [fstMsg, sndMsg] obj
    let newObj' = setBehaviorT (updateMultipleTexts (count + 1)) newObj
    return newObj'

updateTextWithMousePosition :: BehaviorType
updateTextWithMousePosition obj = do
    (Vec2f x y) <- asks (mousePos . input)
    let txt = "mouse: (" ++ show x ++ ", " ++ show y ++ ")"
    let msg = MSG $ setTextMsg txt
    return (setInbox [msg] obj)