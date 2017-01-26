module Kinematics where

import Control.Arrow

type Accel = [Double] -> Double

simpleKinematics :: Double -> Accel -> Accel -> [[Double]] -> [(Double, Double)]
simpleKinematics speed0 distance angle ms = (`zip` map angle ms) $
  map (speed0 +) . tail $ scanl (+) 0 $ map distance ms

trajectory :: ((Double, Double), Double) -> [(Double, Double)]
           -> [((Double, Double), Double)]
trajectory = scanl $ \((x, y), o) (s, a) -> let o' = o + a / 2
  in ((x + s * cos o', y + s * sin o'), o + a)
