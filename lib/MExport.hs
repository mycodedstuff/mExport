module MExport
  ( mExport
  ) where

import Control.Monad
import Prelude

import qualified MExport.Config as CC
import qualified MExport.Parser as LP
import qualified MExport.Pretty as LP
import qualified MExport.Types as MT
import qualified MExport.Utils as LU

mExport :: CC.Config -> IO (MT.Project MT.PrettyModule)
mExport config = do
  let projectPath = CC.projectPath config
  putStrLn $ "Searching for Haskell modules in " ++ projectPath
  modules <- LU.findModules config projectPath
  putStrLn $ "Found " ++ (show $ length modules) ++ " Haskell modules"
  let state = MT.State projectPath modules
  project <- LP.parser config state
  putStrLn $ "Parsed all modules successfully!"
  let prettyModules = LP.prettifyExports config project
  when (CC.writeOnFile config) $ LU.writeExports prettyModules
  return $ MT.Project projectPath prettyModules
