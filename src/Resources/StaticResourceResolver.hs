{-# LANGUAGE TemplateHaskell #-}
module Resources.StaticResourceResolver
    ( getResourceAsString
    , staticResourceMap
    ) where

import Resources.StaticResourceEmbed (resourceDirEmbedded)

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.ByteString.Char8 as BSC

getResourceAsString :: FilePath -> Maybe String
getResourceAsString = (M.!?) staticResourceMap

staticResourceMap :: Map FilePath String
staticResourceMap = M.map BSC.unpack (M.fromList $(resourceDirEmbedded))