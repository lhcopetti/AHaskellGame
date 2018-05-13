module Resources.StaticResourceResolver
    ( getFontPath
    , getSpritePath
    ) where


getFontPath :: String -> String
getFontPath = (++) (rootResourcePath ++ "fonts/")

getSpritePath :: String -> String
getSpritePath = (++) (rootResourcePath ++ "sprites/")

rootResourcePath :: String
rootResourcePath = "/app/resources/"