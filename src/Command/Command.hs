{-# LANGUAGE NamedFieldPuns #-}
module Command.Command
    ( runCommands
    ) where

import GameObject.GameObjectTypes (GameObject (..), Command (..))

runCommands :: GameObject -> GameObject
runCommands obj@GameObject { commands } = let
    goWithoutCommands = cleanCommands obj
    in
        foldr executeCommand goWithoutCommands commands

cleanCommands :: GameObject -> GameObject
cleanCommands obj = obj { commands = [] }

executeCommand :: Command -> GameObject -> GameObject
executeCommand (Command f) = f