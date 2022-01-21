module Types where

import qualified MExport.Config as CC (Config)
import qualified MExport.Types as LT (Context)

data Action
  = ShowVersion
  | Run CC.Config LT.Context
