module Random.Random
    ( printSeed
    , newGenerator
    , createRandomPosition
    , createRandomPositions
    , createRandomSpeed
    , createRandomSpeeds
    , newGeneratorFromString
    ) where

import SFML.System.Vector2 (Vec2f (..), Vec2u (..))

import System.Random (StdGen, getStdGen)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Class (lift)

import Random.RandomState
import GameEnv

newGenerator :: IO StdGen 
newGenerator = do
    gen <- getStdGen
    printSeed gen
    return gen

newGeneratorFromString :: String -> IO StdGen
newGeneratorFromString xs = do
    let gen = read xs :: StdGen
    printSeed gen
    return gen

printSeed :: StdGen -> IO ()
printSeed = putStrLn . ("The SEED is: " ++) . show

createRandomPosition :: ReaderT GameEnvironment (StateT StdGen (MaybeT IO)) Vec2f
createRandomPosition = do
    (Vec2u width height) <- asks gameArea
    x <- lift $ randomRState (0, fromIntegral width)
    y <- lift $ randomRState (0, fromIntegral height)
    return (Vec2f x y)

createRandomPositions :: Int -> ReaderT GameEnvironment (StateT StdGen (MaybeT IO)) [Vec2f]
createRandomPositions = sequence . (`replicate` createRandomPosition)

createRandomSpeed :: Float -> StateT StdGen (MaybeT IO) Vec2f
createRandomSpeed modulo = do
    x <- randomRState interval
    y <- randomRState interval
    return (Vec2f x y)
        where
            interval = (-modulo, modulo)

createRandomSpeeds :: Float -> Int -> StateT StdGen (MaybeT IO) [Vec2f]
createRandomSpeeds vel = sequence . (`replicate` createRandomSpeed vel)