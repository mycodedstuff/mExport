module MExport
  ( mExport
  ) where

import Control.Monad
import Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as DT
import qualified System.FilePath as SF

import qualified MExport.Config as CC
import qualified MExport.Parser as LP
import qualified MExport.Pretty as LP
import qualified MExport.Types as LT
import qualified MExport.Utils as LU

mExport :: CC.Config -> LT.Context -> IO (HM.HashMap String DT.Text)
mExport config context = do
  let moduleSrc = LT.moduleSrc context
      mainSrcDir = SF.takeDirectory moduleSrc
  exportMap <- LP.parser config moduleSrc
  let formattedExports = LP.prettyPrint config <$> exportMap
  when (CC.writeOnFile config) $ LU.writeExports formattedExports mainSrcDir
  return formattedExports
