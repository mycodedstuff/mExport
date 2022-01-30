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
  T.Run <$> (mkConfig <$> projectPath <*> analyze <*> indent <*> collapse)
  where
    projectPath = strOption (long "path" <> help "Path of Haskell project" <> showDefault <> value "." <> metavar "DIR")
    analyze = switch (long "analyze" <> help "Analyze the Haskell project, helps in verifying if project can be parsed")
    indent = option auto (long "indent" <> help "Indentation for the exports" <> showDefault <> value 2 <> metavar "NUM")
    collapse =
      option
        auto
        (long "collapse" <>
         help "Exports everything of a type if NUM % is exported" <> showDefault <> value 0 <> metavar "NUM")
    mkConfig path _analyze _indent _collapse =
      config
        { CC.projectPath = path
        , CC.writeOnFile = not _analyze
        , CC.codeStyle = CC.CodeStyle {CC.indent = _indent, CC.collapseAfter = _collapse}
        }
