module Utils
  ( getAction
  ) where

import Options.Applicative
import Prelude

import qualified MExport.Config as CC
import qualified Types as T

getAction :: CC.Config -> IO T.Action
getAction config =
  execParser (info (options config <**> helper) (header "mExport - minimize export list of haskell modules"))

options :: CC.Config -> Parser T.Action
options config =
  flag' T.ShowVersion (long "version" <> help "Print the version") <|>
  T.Run <$> (mkConfig <$> projectPath <*> analyze <*> ghcParser)
  where
    projectPath = strOption (long "path" <> help "Path of Haskell project" <> showDefault <> value "." <> metavar "DIR")
    analyze = switch (long "analyze" <> help "Analyze the Haskell project")
    ghcParser = switch (long "ghc-parser" <> help "Use ghc-lib-parser instead of haskell-src-exts")
    mkConfig path _analyze ghc = config {CC.projectPath = path, CC.writeOnFile = not _analyze, CC.useGhcParser = ghc}
