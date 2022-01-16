module Types where

import qualified Config.Config as CC (Config)

data Context =
  Context
    { moduleSrc :: String
    }

data Action
  = ShowVersion
  | Run CC.Config Context
