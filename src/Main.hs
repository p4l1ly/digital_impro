{-# LANGUAGE TupleSections, Arrows, TypeFamilies, RankNTypes, LambdaCase #-}

module Main where

import Control.Arrow
import System.Random
import System.FilePath
import System.Directory
import FRP.Yampa
import Graphics.QML
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

import MotorGenerator
import Kinematics
import qualified Defaults as D

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
  roomV <- newMVar ""
  trajV <- newMVar ([] :: [[Double]])

  trajSig <- newSignalKey :: IO (SignalKey (IO ()))
  roomSig <- newSignalKey :: IO (SignalKey (IO ()))
  confSig <- newSignalKey :: IO (SignalKey (IO ()))

  minLenV <- newMVar D.minLength
  maxLenV <- newMVar D.maxLength

  fireCtxSigV <- newEmptyMVar
  let fireCtxSig sig = join $ ($ sig) <$> readMVar fireCtxSigV

      guiActuate :: ReactHandle Input Output -> Bool -> Output -> IO Bool
      guiActuate _ _ (O endE nextE roomE) = do
        flip (event (return ())) nextE $ \traj -> do
          let flatten ((x, y), ((r, g, b, a), w)) =
                [x, y, r, g, b, a, w]
          void $ swapMVar trajV $ map flatten traj
          fireCtxSig trajSig

        flip (event (return ())) roomE $ \roomPath -> do
          swapMVar roomV roomPath
          fireCtxSig roomSig

        return $ isEvent endE

  pwd <- T.pack <$> getCurrentDirectory
  cfgPath <- getDataFileName "cfg/Config.hs"
  putStrLn cfgPath

  style <- (<$> recompileConfig cfgPath) $ \case
    Just style -> style
    Nothing -> return . map (\_ -> ((0, 0, 0, 0.3), 2))

  guiSf' <- guiSf style
  reactHandle <- reactInit (return noI) guiActuate . loop $
    second (iPre ranGen) >>> AS.runState guiSf'

  let react' evs = modifyMVar_ timeV $ \t -> do
        react reactHandle (t, Just evs)
        return 0

      params = ["minLength", "maxLength", "initVelocity", "accelerationFormula",
                "angularAccelerationFormula"]

  (failVs, confProps) <- (unzip <$>) . forM params $ \p -> do
    var <- newMVar False
    return ((p, var), defPropertySigRO' p confSig $ \_ -> readMVar var)

  let check :: String -> (Event a -> Input) -> MaybeT IO a -> IO ()
      check param = \input check_ -> do
        result <- runMaybeT check_
        case result of
          Just x -> do
            swapMVar var False
            fireCtxSig confSig
            react' (input $ Event x)
          _ -> do
            swapMVar var True
            fireCtxSig confSig

        where Just var = lookup param failVs

      defaults =
        [ ("defMinLength"                  , show D.minLength)
        , ("defMaxLength"                  , show D.maxLength)
        , ("defInitVelocity"               , show D.initVelocity)
        , ("defAccelerationFormula"        , D.accelerationFormula)
        , ("defAngularAccelerationFormula" , D.angularAccelerationFormula) ]

      defaultProps = (`map` defaults) $ \(a, b) ->
        defPropertyConst' a $ \_ -> return $ T.pack b

      checkFormula :: T.Text -> T.Text -> IO ()
      checkFormula a aa =
        check "accelerationFormula" (\e -> noI{accelEvent = e}) $
          compileFormulae (T.unpack a) (T.unpack aa)

  ctxClass <- newClass $
    [ defPropertyConst' "currentDirectory" $ \_ -> return pwd

    , defMethod' "nextTrajectory" $ \_ -> do
        react' noI{nextEvent = Event ()}

    , defMethod' "loadWall"       $ \_ path -> do
        let path'       = drop 7 $ T.unpack path
            problem str = putStrLn str >> return False

        mimg <- readImage path'
        case mimg of
          Right (ImageY8 img) -> do
            let (w, h) = fromIntegral *** fromIntegral
                       $ imageWidth &&& imageHeight $ img
                room x y = ifInside (fromIntegral p / 255) x y
                  where p = pixelAt img (floor $ x * w) (floor $ y * h)

            react' noI{roomEvent = Event (path', room)}
            return True

          Right _  -> problem $ "Error: Image should be RGB8 bitmap"
          Left str -> problem $ "Error reading image:\n" ++ str

    , defMethod' "initialize" $ \objRef ->
        fireCtxSigV `putMVar` (`fireSignal` objRef)

    , defMethod' "quit" $ \_ ->
        react' noI{endEvent = Event ()}

    , defPropertySigRO' "trajectory" trajSig $ \_ -> readMVar trajV
    , defPropertySigRO' "roomPath" roomSig $ \_ -> T.pack <$> readMVar roomV

    , defMethod' "setMinLength" $ \_ str ->
        check "minLength" (\e -> noI{minLenEvent = e}) $ do
          l <- wrap . readMaybe $ T.unpack str
          when (l < 0) fail'
          maxLen <- liftIO $ readMVar maxLenV
          when (l > maxLen) fail'
          liftIO $ swapMVar minLenV l
          return l

    , defMethod' "setMaxLength" $ \_ str ->
        check "maxLength" (\e -> noI{maxLenEvent = e}) $ do
          l <- wrap . readMaybe $ T.unpack str
          minLen <- liftIO $ readMVar minLenV
          when (l < minLen) fail'
          liftIO $ swapMVar maxLenV l
          return l

    , defMethod' "setInitVelocity" $ \_ str ->
        check "initVelocity" (\e -> noI{speed0Event = e}) $ do
          s <- wrap . readMaybe $ T.unpack str
          return s

    , defMethod' "setAccelerationFormula" $ \_ -> checkFormula
    , defMethod' "setAngularAccelerationFormula" $ \_ -> checkFormula

    , defMethod' "recompileConfig" $ \_ ->
        recompileConfig cfgPath >>= \case
          Just style -> react' noI{cfgEvent = Event style}
          _ -> return ()

    ] ++ confProps ++ defaultProps

  ctx <- newObject ctxClass ()
  qmlPath <- getDataFileName "qml/Main.qml"
  runEngineLoop
    defaultEngineConfig
      { initialDocument = fileDocument qmlPath
      , contextObject   = Just $ anyObjRef ctx }

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
