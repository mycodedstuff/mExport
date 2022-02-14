module Types where

import qualified Data.Maybe as DM
import qualified Data.Yaml as Y

import qualified MExport.Config as MC

data Action
  = ShowVersion
  | Run MExportArgs

data MExportConfig =
  MExportConfig
    { indent :: Int
    , collapse :: Int
    , extensions :: [String]
    , dumpDir :: Maybe String
    }
  deriving (Show)

instance Y.FromJSON MExportConfig where
  parseJSON (Y.Object v) =
    let defaultCodeStyle = MC.codeStyle MC.defaultConfig
     in MExportConfig <$> fmap (DM.fromMaybe (MC.indent defaultCodeStyle)) (v Y..:? "indent") <*>
        fmap (DM.fromMaybe (MC.collapseAfter defaultCodeStyle)) (v Y..:? "collapse") <*>
        fmap (DM.fromMaybe $ MC.extensions MC.defaultConfig) (v Y..:? "extensions") <*>
        fmap (DM.fromMaybe (MC.dumpDir MC.defaultConfig)) (v Y..:? "dump-dir")
  parseJSON _ = fail "Expected Object for Config value"

data MExportArgs =
  MExportArgs
    { projectPathArg :: String
    , analyseArg :: Bool
    , extensionsArg :: Maybe [String]
    , dumpDirArg :: Maybe String
    , indentArg :: Maybe Int
    , collapseArg :: Maybe Int
    }
  deriving (Show)
