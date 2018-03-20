{-# LANGUAGE NamedFieldPuns #-}
module System.Input.InputSnapshot
    ( InputSnapshot (..)
    , stepSnapshot
    , emptySnapshot
    ) where

import Data.Maybe (catMaybes)

import SFML.Window.Keyboard (KeyCode)
import SFML.Window.Event (SFEvent (..))

import qualified System.Input.MouseSnapshot as MS
import qualified System.Input.KeyboardSnapshot as KS

data InputSnapshot = InputSnapshot  { pressed :: [KeyCode]
                                    , keyboard :: KS.KeyboardSnapshot
                                    , mouse :: MS.MouseSnapshot
                                    } deriving (Show)

emptySnapshot :: InputSnapshot
emptySnapshot = InputSnapshot { pressed = [], mouse = MS.emptySnapshot, keyboard = KS.mkKeyboardSnapshot }

stepSnapshot :: InputSnapshot -> [SFEvent] -> InputSnapshot
stepSnapshot snap evts = InputSnapshot 
    { pressed = getPressedKeys evts
    , keyboard = KS.stepKeyboardSnapshot (keyboard snap) evts
    , mouse = MS.stepMouseSnapshot (mouse snap) evts
    }

getPressedKeys :: [SFEvent] -> [KeyCode]
getPressedKeys = catMaybes . fmap getPressedKey

getPressedKey :: SFEvent -> Maybe KeyCode
getPressedKey SFEvtKeyPressed { code } = Just code
getPressedKey _ = Nothing