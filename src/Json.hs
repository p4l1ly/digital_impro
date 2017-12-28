{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Json where

import GHC.Exts
import GHC.Generics
import Data.Aeson hiding (Error)
import Data.Text hiding (map, zipWith, tail)

data Cmd = Cmd
  { event :: !Text
  } deriving (Show, Generic)

instance FromJSON Cmd

data Error = Error
  { error :: !Text
  } deriving (Show, Generic)

instance ToJSON Error

err = Error . pack

newtype Trajectory = Traj
  [((Double, Double), ((Double, Double, Double, Double), Double))]

instance ToJSON Trajectory where
  toJSON (Traj pts) =
    toJSON $ zipWith format pts . tail $ map fst pts
    where format ((x1, y1), ((r, g, b, a), w)) (x2, y2) = object
            [ "shape" .= ("Line" :: Text)
            , "args" .=
                [ toJSON [x1, y1, x2, y2]
                , object
                    [ "stroke" .= toJSON [r, g, b, a]
                    , "strokeWidth" .= w
                    ]
                ]
            ]
