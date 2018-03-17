{-# LANGUAGE NamedFieldPuns #-}
module Command.Command
    ( runCommands
    ) where

import Control.Monad (foldM)

import GameObject.GameObjectTypes

runCommands :: GoUpdateType
runCommands obj@GameObject { commands } = do
    let goWithoutCommands = cleanCommands obj
    foldM (flip executeCommand) goWithoutCommands commands

cleanCommands :: GameObject a -> GameObject a
cleanCommands obj = obj { commands = [] }

executeCommand :: Command -> GoUpdateType
executeCommand (Command f) = f