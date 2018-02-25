module Main where


import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO)

import Conway

main = do
    putStrLn "The Conway-app"
    putStrLn "Welcome to the Conway-app!"
    runMaybeT $ do
        w <- MaybeT . return . newConwayWorld $ (2, 3)
        liftIO $ putStrLn $ "The board is: \n" ++ show w
        let w' = setLive (0, 1) w
        liftIO $ putStrLn $ "The board is: \n" ++ show w'
        let cell = cellAt (0, 1) w'
        liftIO $ putStrLn $ "This is the cell at (0, 1): " ++ show cell
    return ()