{-# LANGUAGE NamedFieldPuns #-}
module Command.Command
    ( runCommands
    ) where

import Control.Monad (foldM)

import GameObject.GameObjectTypes (GameObject (..), Command (..))
import Updatable (UpdateType)

runCommands :: UpdateType GameObject
runCommands obj@GameObject { commands } = do
    let goWithoutCommands = cleanCommands obj
    foldM (flip executeCommand) goWithoutCommands commands

cleanCommands :: GameObject -> GameObject
cleanCommands obj = obj { commands = [] }

executeCommand :: Command -> UpdateType GameObject
executeCommand (Command f) = f