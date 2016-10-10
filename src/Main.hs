{-# LANGUAGE TupleSections, Arrows #-}

module Main where

import System.Environment
import Control.Arrow
import System.Random
import System.FilePath
import Control.Monad
import Control.Monad.State
import Codec.Picture
import Graphics.Rendering.Cairo

import MotorGenerator
import Kinematics

outside :: Double -> Double -> Bool
outside x y = x < 0 || x >= 1 || y < 0 || y >= 1

type Room  = Double -> Double -> Bool
type Traj  = [((Double, Double), Double)] -- trajectory

genTraj :: Room -> State StdGen [(Double, Double)]
genTraj room = do
  ms <- generateMotors

  [x, y] <- replicateM 2 . state $ randomR (0, 1)
  a <- state $ randomR (-pi, pi)

  let result = map fst . trajectory ((x, y), a) $ simpleKinematics ms
  if all (\(x, y) -> not $ room x y) result
    then return result
    else genTraj room

readImg :: FilePath -> IO Room
readImg path = do
  Right (ImageY8 img) <- readImage path
  let (w, h) = fromIntegral *** fromIntegral
             $ imageWidth &&& imageHeight $ img
      room x y = outside x y || p < 128
        where p = pixelAt img (floor $ x * w) (floor $ y * h)
  return room

(outW, outH) = (1000, 1000)

rr = (0.0, 0.2) -- (0.3, 1)
gr = (0.5, 0.7)
br = (0.0, 0.2)

main :: IO ()
main = do
  [infile, outfile] <- getArgs
  room <- readImg infile

  g <- getStdGen
  let (trajs, g') = runState (replicateM 500 genFn) g
      genFn = do
        traj <- genTraj room
        colors <- mapM (state . randomR) [rr, gr, br]
        let t = map (\(x, y) -> (fromIntegral outW * x, fromIntegral outH * y))
                    traj
        return (t, colors)

  setStdGen g'

  withImageSurface FormatARGB32 outW outH $ \surface -> do
    renderWith surface $ do
      setSourceRGBA 0 0 0 0
      fill

      forM trajs $ \(((x, y):rest), [r, g, b]) -> do
        setSourceRGBA r g b 1
        moveTo x y
        mapM (uncurry lineTo) rest
        stroke

    surfaceWriteToPNG surface outfile
