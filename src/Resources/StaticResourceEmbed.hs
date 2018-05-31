module Resources.StaticResourceEmbed
    ( resourceDirEmbedded
    ) where

import Language.Haskell.TH (ExpQ)
import Data.FileEmbed (embedDir)

resourceDirEmbedded :: ExpQ
resourceDirEmbedded = embedDir "resources/"