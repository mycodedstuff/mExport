module MExport.Types where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as DT
import qualified Language.Haskell.Exts as H

type ModuleMap = HM.HashMap String Module

type SpecMap = HM.HashMap String (H.ImportSpec H.SrcSpanInfo)

data Project a =
  Project
    { _rootDir :: String
    , _modules :: [a]
    }
  deriving (Show)

data Module =
  Module
    { _name :: String
    , _path :: String
    , _exportSpecs :: H.ExportSpecList H.SrcSpanInfo
    }
  deriving (Show)

data PrettyModule =
  PrettyModule
    { _name :: String
    , _path :: String
    , _exportList :: DT.Text
    }
  deriving (Show)

data State =
  State
    { _rootDir :: String
    , _modulePaths :: [String]
    }
  deriving (Show)

data MetaModule =
  MetaModule
    { _name :: String
    , _path :: Maybe String
    , _specMap :: SpecMap
    }
  deriving (Show)
