module MExport
  ( mExport
  ) where

import Control.Monad

import qualified MExport.Config as CC
import qualified MExport.Parser as LP
import qualified MExport.Parser.Cabal as PC
import qualified MExport.Pretty as LP
import qualified MExport.Types as MT
import qualified MExport.Utils.Utils as LU
import qualified MExport.Utils.Writer as UW

mExport :: CC.Config -> IO (MT.Project MT.PrettyModule)
mExport config = do
  let projectPath = CC.projectPath config
  cabalFile <- LU.findCabalFile projectPath
  putStrLn $ "Parsing " ++ cabalFile
  projectInfo <- PC.parseCabalFile cabalFile
  project <- LP.parser config projectInfo
  putStrLn $ "Parsed all modules successfully!"
  let prettyModules = LP.prettifyExports config project
  when (CC.writeOnFile config) $ UW.writeExports prettyModules
  return $ MT.Project projectPath prettyModules
