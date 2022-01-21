module Utils
  ( getAction
  ) where

import Options.Applicative

import qualified MExport.Config as CC
import qualified Types as T

getAction :: CC.Config -> IO T.Action
getAction config =
  execParser (info (options config <**> helper) (header "mExport - minimize export list of haskell modules"))

options :: CC.Config -> Parser T.Action
options config = flag' T.ShowVersion (long "version" <> help "Print the version") <|> T.Run <$> (mkConfig <$> projectPath)
  where
    projectPath = strOption (long "path" <> help "Path of Haskell project" <> metavar "DIR")
    mkConfig path = config {CC.projectPath = path}
