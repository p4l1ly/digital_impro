module MotorGenerator where

import System.Random
import Data.List
import Control.Monad.State

generateMotors :: State StdGen [(Double, Double, Double, Double)]
generateMotors = do
  n <- state $ randomR (5, 7)
  [m1, m2, m3, m4] <- replicateM 4 . replicateM n . state $ randomR (-1, 1)
  return $ zip4 m1 m2 m3 m4
