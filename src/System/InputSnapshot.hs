{-# LANGUAGE NamedFieldPuns #-}
module System.InputSnapshot
    ( InputSnapshot (..)
    , createSnapshot
    , emptySnapshot
    , isPressed
    ) where

import Data.Maybe (catMaybes)

import SFML.Window.Keyboard (KeyCode)
import SFML.Window.Event (SFEvent (..))

data InputSnapshot = InputSnapshot  { pressed :: [KeyCode]
                                    } deriving (Show)

emptySnapshot :: InputSnapshot
emptySnapshot = InputSnapshot { pressed = [] }

createSnapshot :: [SFEvent] -> InputSnapshot
createSnapshot xs = InputSnapshot { pressed = getPressedKeys xs }

getPressedKeys :: [SFEvent] -> [KeyCode]
getPressedKeys = catMaybes . fmap getPressedKey


getPressedKey :: SFEvent -> Maybe KeyCode
getPressedKey SFEvtKeyPressed { code } = Just code
getPressedKey _ = Nothing

isPressed :: KeyCode -> InputSnapshot -> Bool
isPressed key InputSnapshot { pressed } = key `elem` pressed