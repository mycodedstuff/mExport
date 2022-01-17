module Lib.MExport
  ( mExport
  ) where

import Control.Monad
import Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as DT
import qualified System.FilePath as SF

import qualified Lib.Config as CC
import qualified Lib.Parser as LP
import qualified Lib.Pretty as LP
import qualified Lib.Types as LT
import qualified Lib.Utils as LU

mExport :: CC.Config -> LT.Context -> IO (HM.HashMap String DT.Text)
mExport config context = do
  let moduleSrc = LT.moduleSrc context
      mainSrcDir = SF.takeDirectory moduleSrc
  exportMap <- LP.parser config moduleSrc
  let formattedExports = LP.prettyPrint config <$> exportMap
  when (CC.writeOnFile config) $ LU.writeExports formattedExports mainSrcDir
  return formattedExports
