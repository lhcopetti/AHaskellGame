module Main where

import Conway

main = do
    putStrLn "The Conway-app"
    putStrLn "Welcome to the Conway-app!"
    let t = newConwayWorld (2, 3)
    putStrLn $ "The board is: \n" ++ show t
    let t' = setLive (0, 1) t
    putStrLn $ "The board is: \n" ++ show t'
    return ()