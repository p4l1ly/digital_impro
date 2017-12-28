{-# LANGUAGE Arrows, LambdaCase, NamedFieldPuns, OverloadedStrings,
             ExtendedDefaultRules #-}

module Main where

import Debug.Trace

import Control.Arrow
import System.Random
import System.FilePath
import System.Directory
import FRP.Yampa
import Control.Concurrent.MVar
import Paths_digital_impro
import Control.Monad
import Control.Monad.State as MS hiding (lift)
import qualified Control.Monad.Trans as MT
import Control.Monad.Trans.Maybe
import Control.Arrow.Transformer.State as AS
import Control.Arrow.Transformer
import Control.Arrow.Operations
import Codec.Picture
import qualified Data.Text as T
import qualified Data.Map  as M
import Text.Read hiding (lift)
import Data.Typeable
import Data.List
import Text.Regex.Posix
import Text.Printf
import Language.Haskell.Interpreter hiding (lift)
import Data.Aeson hiding (unlines)
import qualified Data.ByteString.Lazy.Char8 as B

import MotorGenerator
import Kinematics
import qualified Defaults as D
import qualified Json as J

type Room  = Double -> Double -> Double
type Pt    = (Double, Double)
type Color = (Double, Double, Double, Double)
type Width = Double
type SegmentStyle = [Pt] -> State StdGen [(Color, Width)]

data Input = I{ endEvent    :: Event ()
              , nextEvent   :: Event ()
              , roomEvent   :: Event (String, Room)
              , minLenEvent :: Event Int
              , maxLenEvent :: Event Int
              , speed0Event :: Event Double
              , accelEvent  :: Event (Accel, Accel, Int)
              , cfgEvent    :: Event SegmentStyle
              }
noI = I NoEvent NoEvent NoEvent NoEvent NoEvent NoEvent NoEvent NoEvent

data Output = O (Event ()) (Event [(Pt, (Color, Width))]) (Event String)

ifInside :: Double -> Double -> Double -> Double
ifInside val x y | x < 0 || x >= 1 || y < 0 || y >= 1 = 0
                 | otherwise = val

guiSf :: SegmentStyle -> IO (StateArrow StdGen SF Input Output)
guiSf style0 = do
  Just r <- runMaybeT $
    compileFormulae D.accelerationFormula D.angularAccelerationFormula

  return $ proc (I endE nextE roomE minLenE maxLenE speed0E accelE
                   styleE) -> do
    room <- lift $ hold (ifInside 1) -< snd <$> roomE
    style <- lift $ hold style0 -< styleE

    minLen <- lift $ hold D.minLength -< minLenE
    maxLen <- lift $ hold D.maxLength -< maxLenE
    speed0 <- lift $ hold D.initVelocity -< speed0E
    (distance, angle, motors) <- lift $ hold r -< accelE

    gen <- fetch -< ()
    let (trajE, genE) = splitE . tag nextE $ MS.runState (loop 1000) gen
        loop 0 = return []
        loop i = do
          ms <- generateMotors motors minLen maxLen

          [x, y] <- replicateM 2 . state $ randomR (0, 1)
          a <- state $ randomR (-pi, pi)

          let result = map fst . trajectory ((x, y), a) $
                simpleKinematics speed0 distance angle ms

          treshold <- state $ randomR (0, 1)
          if all (\(x, y) -> room x y > treshold) result
            then zip result <$> style result
            else loop $ i - 1

    store -< event gen id genE

    returnA -< O endE trajE (fst <$> roomE)

main :: IO ()
main = do
  ranGen <- newStdGen

  timeV <- newMVar 0

  let guiActuate :: ReactHandle Input Output -> Bool -> Output -> IO Bool
      guiActuate _ _ (O endE nextE roomE) = do
        flip (event (return ())) nextE $
          putStrLn . B.unpack . encode . J.Traj

        return $ isEvent endE

  pwd <- T.pack <$> getCurrentDirectory
  cfgPath <- getDataFileName "cfg/Config.hs"

  style <- (<$> recompileConfig cfgPath) $ \case
    Just style -> style
    Nothing -> return . map (\_ -> ((0, 0, 0, 0.3), 2))

  guiSf' <- guiSf style
  reactHandle <- reactInit (return noI) guiActuate . loop $
    second (iPre ranGen) >>> AS.runState guiSf'

  let react' evs = modifyMVar_ timeV $ \t -> do
        react reactHandle (t, Just evs)
        return 0

  cmds <- B.getContents
  forM_ (B.lines cmds) $ \x -> do
    case eitherDecode x of
      Left err -> B.putStrLn . encode $ J.err err
      Right x -> jsonToEvent x >>= react'

  react' noI{endEvent = Event ()}

jsonToEvent :: J.Cmd -> IO Input
jsonToEvent J.Cmd{J.event = "generate"} = return noI{nextEvent = Event ()}
jsonToEvent J.Cmd{J.event} = do
  B.putStrLn . encode . J.err $ printf "unknown event %s" event
  return noI

wrap = MaybeT . return

fail' :: MaybeT IO a
fail' = wrap Nothing

prettyShow :: InterpreterError -> String
prettyShow (WontCompile errs) =
  intercalate "\n\n" $ "*** InterpreterError WontCompile" : map errMsg errs
prettyShow (UnknownError err) = "*** Interpreter UnknownError\n\n" ++ err
prettyShow (NotAllowed err)   = "*** Interpreter NotAllowed\n\n"   ++ err
prettyShow (GhcException err) = "*** Interpreter GhcException\n\n" ++ err

compileFormulae :: String -> String -> MaybeT IO (Accel, Accel, Int)
compileFormulae a aa = do
  r <- liftIO . runInterpreter $ do
    setImports ["Prelude"]
    interpret code (as :: [[Double] -> Double])

  case r of
    Right [acc, aacc] -> return (acc, aacc, length ids')
    Left err -> liftIO (putStrLn $ prettyShow err) >> fail'

  where
    ids = map (!! 2) $ (a ++ " " ++ aa) =~
      "(^|[^a-zA-Z0-9_])([a-zA-Z_][a-zA-Z0-9_]*)"

    ids' = map head . group $ sort ids

    genFn name expr = unlines
      [ printf "  %s :: [Double] -> Double" name
      , printf "  %s __xs__ = %s" name expr
      , printf "    where [%s] = __xs__" $ intercalate ", " ids' ]

    code = unlines [ "let", genFn "__acc__" a
                          , genFn "__aacc__" aa
                   , "in [__acc__, __aacc__]" ]

recompileConfig :: FilePath -> IO (Maybe SegmentStyle)
recompileConfig cfgPath = do
  r <- runInterpreter $ do
    loadModules [cfgPath]
    setImports ["Prelude"]
    setTopLevelModules ["Config"]
    interpret "segmentStyle" (as :: SegmentStyle)

  case r of
    Right style -> return $ Just style
    Left err -> do putStrLn $ prettyShow err
                   return Nothing
