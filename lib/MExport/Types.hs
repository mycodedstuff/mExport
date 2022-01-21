module MExport.Types where

import qualified Data.HashMap.Strict as DM
import qualified Language.Haskell.Exts as H

type ModuleMap = DM.HashMap String (DM.HashMap String (H.ExportSpec H.SrcSpanInfo))

data Module =
  Module
    { name :: Maybe String
    , path :: String
    }
  deriving (Show)

data Project =
  Project
    { modules :: [Module]
    , rootDir :: String
    }
  deriving (Show)
