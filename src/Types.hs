module Types where

import qualified Lib.Config as CC (Config)
import qualified Lib.Types as LT (Context)

data Action
  = ShowVersion
  | Run CC.Config LT.Context
