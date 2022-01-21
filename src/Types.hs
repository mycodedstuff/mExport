module Types where

import qualified MExport.Config as CC (Config)

data Action
  = ShowVersion
  | Run CC.Config
