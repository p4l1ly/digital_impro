{-# LANGUAGE TupleSections #-}

module Config where

import Control.Monad.Identity
import Control.Monad.State
import System.Random
import Data.List

type In = [(Double, Double)]
type Out = State StdGen [((Double, Double, Double, Double), Double)]
type SegStyle = In -> Out

rand :: Random a => (a, a) -> State StdGen a
rand = state . randomR

segmentStyle :: SegStyle
segmentStyle = semitransparent
  where
    hair = return . map (\_ -> ((0, 0, 0, 1), 1)) :: SegStyle
    semitransparent = return . map (\_ -> ((0, 0, 0, 0.3), 2)) :: SegStyle

    sperm :: SegStyle
    sperm xs = return $ zipWith (\a w -> ((0, 0, 0, a), w))
      [1, 1 - 1 / fromIntegral (length xs) .. 0]
      ([4, 6 .. 9] ++ [9, 8.2 .. 2] ++ repeat 2)

    worm :: SegStyle
    worm xs = do
      colorChanges <- replicateM l . replicateM 3 $ rand (-0.01, 0.01)

      initColor <- mapM rand
        [(135/255, 141/255), (65/255, 77/255), (0, 12/255)]

      let colors = map (\[r, g, b] -> (r, g, b, 1))
                 $ scanl (zipWith (+)) initColor colorChanges

      extreme <- rand (l `div` 3, 2 * l `div` 3)

      let widths = [7, 7 + 5 / fromIntegral extreme .. 12]
                ++ [12, 12 - 5 / fromIntegral (l - extreme) .. 7]

      return $ zip colors widths

      where l = length xs
