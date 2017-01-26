module MotorGenerator where

import System.Random
import Data.List
import Control.Monad.State

generateMotors :: Int -> Int -> Int -> State StdGen [[Double]]
generateMotors motors from to = do
  iters <- state $ randomR (from, to)
  replicateM iters . replicateM motors . state $ randomR (-1, 1)
