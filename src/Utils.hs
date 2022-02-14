module Utils
  ( getAction
  , mkConfig
  , version
  ) where

import Options.Applicative
import Prelude

import qualified Data.Maybe as DM
import qualified Data.Text as DT
import qualified Data.Version as DV
import qualified Data.Yaml as Y
import qualified System.Directory as SD
import qualified System.FilePath as SF

import qualified MExport.Config as MC
import qualified MExport.Utils.Utils as MU
import qualified Types as T

import qualified Paths_mExport as PM (version)

getAction :: IO T.Action
getAction =
  execParser (info (options <**> helper) (header ("mExport " <> version <> " - minimize export list of haskell modules")))

options :: Parser T.Action
options =
  flag' T.ShowVersion (long "version" <> help "Print the version") <|>
  T.Run <$> (mkArgs <$> projectPath <*> analyze <*> customExtensions <*> dumpDir <*> indent <*> collapse)
  where
    projectPath = strOption (long "path" <> help "Path of Haskell project" <> showDefault <> value "." <> metavar "DIR")
    analyze = switch (long "analyze" <> help "Analyze the Haskell project, helps in verifying if project can be parsed")
    customExtensions =
      optional $ strOption (long "extensions" <> help "Comma separated GHC Language extensions" <> metavar "GHCEXT")
    mkExtensions strExts = DT.unpack <$> (DT.splitOn "," $ DT.pack strExts)
    indent = optional $ option auto (long "indent" <> help "Indentation for the exports" <> metavar "NUM")
    collapse =
      optional $
      option
        auto
        (long "collapse" <> help "Exports everything of a type if NUM or more percentage is exported" <> metavar "NUM")
    dumpDir = optional $ strOption (long "dump-dir" <> help "GHC dump directory path" <> metavar "DIR")
    mkArgs path _analyze extensions _dumpDir _indent _collapse =
      T.MExportArgs
        { T.projectPathArg = path
        , T.analyseArg = _analyze
        , T.extensionsArg = mkExtensions <$> extensions
        , T.dumpDirArg = _dumpDir
        , T.indentArg = _indent
        , T.collapseArg = _collapse
        }

mkConfig :: MC.Config -> T.MExportArgs -> IO MC.Config
mkConfig defaultConfig T.MExportArgs {..} = do
  updatedConfig <- getConfigFromYaml defaultConfig projectPathArg
  let updatedCodeStyle = MC.codeStyle updatedConfig
  return $
    updatedConfig
      { MC.projectPath = projectPathArg
      , MC.writeOnFile = not analyseArg
      , MC.extensions = DM.fromMaybe (MC.extensions updatedConfig) extensionsArg
      , MC.dumpDir = dumpDirArg <|> (MC.dumpDir updatedConfig)
      , MC.codeStyle =
          MC.CodeStyle
            { MC.indent = DM.fromMaybe (MC.indent updatedCodeStyle) indentArg
            , MC.collapseAfter = DM.fromMaybe (MC.collapseAfter updatedCodeStyle) collapseArg
            }
      }

getConfigFromYaml :: MC.Config -> String -> IO MC.Config
getConfigFromYaml defaultConfig path = do
  homeDir <- SD.getHomeDirectory
  maybeConfig <- findConfigFile homeDir =<< SD.makeAbsolute path
  case maybeConfig of
    Nothing -> return defaultConfig
    Just file -> do
      result <- Y.decodeFileEither file
      case result of
        Left e -> error (show e)
        Right config -> applyConfig file config
  where
    findConfigFile :: String -> String -> IO (Maybe String)
    findConfigFile homeDir curDir = do
      if homeDir /= curDir
        then do
          maybeConfig <- return . MU.headMaybe . filter (== ".mexport.yaml") =<< SD.listDirectory curDir
          case maybeConfig of
            Just configFile -> return $ Just $ curDir SF.</> configFile
            Nothing -> findConfigFile homeDir $ SF.takeDirectory curDir
        else return Nothing
    applyConfig :: String -> T.MExportConfig -> IO MC.Config
    applyConfig (SF.takeDirectory -> configDir) T.MExportConfig {..} = do
      let defaultCodeStyle = MC.codeStyle defaultConfig
          abDumpDir =
            case dumpDir of
              Just dir ->
                if SF.isAbsolute dir
                  then Just dir
                  else Just $ SF.normalise $ configDir SF.</> dir
              Nothing -> Nothing
      return $
        defaultConfig
          { MC.codeStyle = defaultCodeStyle {MC.indent = indent, MC.collapseAfter = collapse}
          , MC.extensions = extensions
          , MC.dumpDir = abDumpDir
          }

version :: String
version = DV.showVersion PM.version
