module Resources.StaticResourceEmbed
    ( resourceDirEmbedded
    ) where

import Language.Haskell.TH (ExpQ, runIO)
import Data.FileEmbed (embedDir)

import Paths_AHaskellGame (getDataDir)

resourceDirEmbedded :: ExpQ
resourceDirEmbedded = do
    dataDir <- runIO getDataDir
    embedDir (dataDir ++ "/resources/")