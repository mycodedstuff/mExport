module MExport.Types where

import qualified Data.HashMap.Strict as DM
import qualified Language.Haskell.Exts as H

type ModuleMap = DM.HashMap String (DM.HashMap String (H.ExportSpec H.SrcSpanInfo))

data Context =
  Context
    { moduleSrc :: String
    }
