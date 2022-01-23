module MExport.Accessor where

import Control.Lens (makeLenses)

import qualified MExport.Types as MT

makeLenses ''MT.MetaModule
