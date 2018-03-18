{-# LANGUAGE NamedFieldPuns #-}
module System.InputSnapshot
    ( InputSnapshot (..)
    , stepSnapshot
    , emptySnapshot
    , isPressed
    ) where

import Data.Maybe (catMaybes)

import SFML.Window.Keyboard (KeyCode)
import SFML.Window.Event (SFEvent (..))

import qualified System.MouseSnapshot as MS

data InputSnapshot = InputSnapshot  { pressed :: [KeyCode]
                                    , mouse :: MS.MouseSnapshot
                                    } deriving (Show)

emptySnapshot :: InputSnapshot
emptySnapshot = InputSnapshot { pressed = [], mouse = MS.emptySnapshot }

stepSnapshot :: InputSnapshot -> [SFEvent] -> InputSnapshot
stepSnapshot snap evts = InputSnapshot 
    { pressed = getPressedKeys evts
    , mouse = MS.stepMouseSnapshot (mouse snap) evts
    }

getPressedKeys :: [SFEvent] -> [KeyCode]
getPressedKeys = catMaybes . fmap getPressedKey

getPressedKey :: SFEvent -> Maybe KeyCode
getPressedKey SFEvtKeyPressed { code } = Just code
getPressedKey _ = Nothing

isPressed :: KeyCode -> InputSnapshot -> Bool
isPressed key InputSnapshot { pressed } = key `elem` pressed