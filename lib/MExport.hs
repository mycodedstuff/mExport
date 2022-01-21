module MExport
  ( mExport
  ) where

import Control.Monad
import Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as DT

import qualified MExport.Config as CC
import qualified MExport.Parser as LP
import qualified MExport.Pretty as LP
import qualified MExport.Utils as LU

mExport :: CC.Config -> IO (HM.HashMap String DT.Text)
mExport config = do
  let projectPath = CC.projectPath config
  project <- LU.readProject projectPath
  exportMap <- LP.parser config project
  let formattedExports = LP.prettyPrint config <$> exportMap
  when (CC.writeOnFile config) $ LU.writeExports formattedExports projectPath
  return formattedExports
