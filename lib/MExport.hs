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

import qualified MExport.Ghc.Parser as MGP

mExport :: CC.Config -> IO (MT.Project MT.PrettyModule)
mExport config = do
  let projectPath = CC.projectPath config
      useGhcParser = CC.useGhcParser config
  putStrLn $ "Searching for Haskell modules in " ++ projectPath
  modules <- LU.findModules config projectPath
  putStrLn $ "Found " ++ (show $ length modules) ++ " Haskell modules"
  let state = MT.State projectPath modules
  prettyModules <-
    if useGhcParser
      then do
        project <- MGP.parser config state
        putStrLn $ "Parsed all modules successfully!"
        return $ LP.prettifyExports config project
      else do
        project <- LP.parser config state
        putStrLn $ "Parsed all modules successfully!"
        return $ LP.prettifyModuleExports config project
  when (CC.writeOnFile config) $ LU.writeExports prettyModules
  return $ MT.Project projectPath prettyModules
