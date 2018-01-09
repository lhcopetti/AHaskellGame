module Component.Draw.DrawingHelper
    ( createShapeT
    ) where

import SFML.SFException

import Control.Monad (mzero)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.IO.Class (liftIO)

createShapeT :: IO (Either SFException a) -> MaybeT IO a
createShapeT f = do
    shape <- liftIO f
    case shape of 
        Left e -> liftIO (putStrLn $ "Error while creating a shape. " ++ show e) >> mzero
        Right r -> return r