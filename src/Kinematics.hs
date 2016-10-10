module Kinematics where

import Control.Arrow

simpleKinematics :: [(Double, Double, Double, Double)] -> [(Double, Double)]
simpleKinematics = map (speed &&& angle)
  where speed (m1, m2, m3, m4) = startSpeed + speedCoef * (m1 + m2 - m3 - m4)
        angle (m1, m2, m3, m4) = angleCoef * (m2 + m4 - m1 - m3)

        speedCoef  = 0.0030
        angleCoef  = 0.4
        startSpeed = 0.0050

trajectory :: ((Double, Double), Double) -> [(Double, Double)]
           -> [((Double, Double), Double)]
trajectory = scanl $ \((x, y), o) (s, a) -> let o' = o + a / 2
  in ((x + s * cos o', y + s * sin o'), o + a)
