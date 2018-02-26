module Main where


import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO)

import Conway

main = do
    putStrLn "The Conway-app"
    putStrLn "Welcome to the Conway-app!"
    runMaybeT $ do
        -- This is bugged when the size is (10, 5). Trust me!
        w <- MaybeT . return . newConwayWorld $ (10, 5)
        -- w <- MaybeT . return . newConwayWorld $ (5, 5)
        let setLives :: [Position] -> ConwayWorld -> ConwayWorld; 
            setLives xs w = foldr setLive w xs
        let w' = setLives [(1, 2), (2, 2), (3, 2)] w
        liftIO $ putStrLn $ "The board is: \n" ++ show w'
        let w'' = tick w'
        liftIO $ putStrLn $ "The board is: \n" ++ show w''
        let ww' = tick w''
        liftIO $ putStrLn $ "The board is: \n" ++ show ww'
        let ww'' = tick ww'
        liftIO $ putStrLn $ "The board is: \n" ++ show ww''
    return ()