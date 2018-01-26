{-# LANGUAGE RankNTypes #-}
module Component.Behavior.TextBehavior
    ( updatePromptForGOCount
    , updateTextWithMousePosition
    , updateMultipleTexts
    ) where

import SFML.System.Vector2 (Vec2f (..))

import Control.Monad (liftM)
import Control.Monad.Reader (asks)

import GameObject.GameObject ()
import Component.Behavior.Behavior
import System.Messaging.MessageHelper (pushMessage, pushNamedMessage)
import GameEnv (GameEnvironment (..))
import Input.Mouse (mousePos)
import System.Messaging.TextDrawingMessage (setTextMsg)

updatePromptForGOCount :: String -> BehaviorType
updatePromptForGOCount prompt obj = do
    numberOfGameObjects <- asks countGOs
    pushMessage (setTextMsg (prompt ++ ": " ++ show numberOfGameObjects)) obj

updateMultipleTexts :: Int -> BehaviorType
updateMultipleTexts count obj = liftM normalFunction monadic
    where
        monadic = pushNamedMessage "title"    (setTextMsg $ "This is the title:" ++ show count) obj >>=
                  pushNamedMessage "subtitle" (setTextMsg $ "This is the subtitle:" ++ show count)
        normalFunction = setBehaviorT (updateMultipleTexts (count + 1))


updateTextWithMousePosition :: BehaviorType
updateTextWithMousePosition obj = do
    (Vec2f x y) <- asks (mousePos . input)
    let txt = "mouse: (" ++ show x ++ ", " ++ show y ++ ")"
    pushMessage (setTextMsg txt) obj