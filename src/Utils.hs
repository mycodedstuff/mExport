module Utils
  ( getAction
  ) where

import Options.Applicative

import qualified MExport.Config as CC
import qualified MExport.Types as LT

import qualified Types as T

getAction :: CC.Config -> IO T.Action
getAction config =
  execParser (info (options config <**> helper) (header "mExport - minimize export list of haskell modules"))

options :: CC.Config -> Parser T.Action
options config =
  flag' T.ShowVersion (long "version" <> help "Print the version") <|> T.Run config <$> (LT.Context <$> moduleSrc)
  where
    moduleSrc = strOption (long "src" <> help "Source file of the module" <> metavar "FILE")
