module Command.Commands
    ( dieCommand
    ) where


import GameObject.GameObjectTypes
import GameObject.GameObject (addCommand)

import Killable (die)

dieCommand :: GoUpdateType
dieCommand obj = let
    comm = Command (return . die)
    in
        return (addCommand comm obj)