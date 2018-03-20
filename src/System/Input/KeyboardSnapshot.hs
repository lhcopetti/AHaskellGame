{-# LANGUAGE NamedFieldPuns #-}
module System.Input.KeyboardSnapshot
    ( KeyboardSnapshot
    , mkKeyboardSnapshot
    , reduceEvents
    , toKeyEvent
    , unifyMaps
    , stepKeyboard
    , stepKeyboardSnapshot
    ) where

import SFML.Window.Event (SFEvent (..))
import SFML.Window.Keyboard
        
import qualified Data.Map as M

import qualified System.Input.InputState as I
import ExtSFML.SFMLInstances ()

data KeyboardSnapshot = KeyboardSnapshot    { keys :: M.Map KeyCode I.State
                                            } deriving (Eq, Show)

mkKeyboardSnapshot :: KeyboardSnapshot
mkKeyboardSnapshot = KeyboardSnapshot M.empty

-- getKey :: KeyCode -> KeyboardSnapshot -> I.State
-- getKey key KeyboardSnapshot { keys } = M.findWithDefault I.emptyState key keys

reduceEvents :: [SFEvent] -> M.Map KeyCode [I.Event]
reduceEvents = foldr go M.empty
    where
        go evt = insertWithMaybe' (toKeyEvent evt)

insertWithMaybe' :: Maybe (KeyCode, I.Event) -> M.Map KeyCode [I.Event] -> M.Map KeyCode [I.Event]
insertWithMaybe' Nothing mmap = mmap
insertWithMaybe' (Just (k, v)) mmap = M.insertWith (++) k [v] mmap

toKeyEvent :: SFEvent -> Maybe (KeyCode, I.Event)
toKeyEvent (SFEvtKeyPressed  k _ _ _ _) = Just (k, I.Pressed)
toKeyEvent (SFEvtKeyReleased k _ _ _ _) = Just (k, I.Released)
toKeyEvent _ = Nothing

stepKeyboardSnapshot :: KeyboardSnapshot -> [SFEvent] -> KeyboardSnapshot
stepKeyboardSnapshot KeyboardSnapshot { keys } evts = KeyboardSnapshot { keys = stepKeyboard keys evts }

unifyMaps :: M.Map KeyCode I.State -> M.Map KeyCode [I.Event] -> M.Map KeyCode I.State
unifyMaps mState mEvents = foldr go mState (M.keys mEvents)
    where
        go key = M.insertWith (flip const) key I.emptyState

stepKeyboard :: M.Map KeyCode I.State -> [SFEvent] -> M.Map KeyCode I.State
stepKeyboard mmap evts = M.mapWithKey (go mapEvents) unifiedMap
    where
        mapEvents = reduceEvents evts
        unifiedMap = unifyMaps mmap mapEvents
        go evtMapping key value = foldr (flip I.stepState) value eventList
            where
                eventList = M.findWithDefault [I.Nil] key evtMapping

