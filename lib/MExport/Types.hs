module MExport.Types where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as DT
import qualified Language.Haskell.Exts as H

type ModuleMap = HM.HashMap String Module

type SpecMap = HM.HashMap String (H.ExportSpec H.SrcSpanInfo)

data Project a =
  Project
    { rootDir :: String
    , modules :: [a]
    }
  deriving (Show)

data Module =
  Module
    { name :: String
    , path :: String
    , exportSpecs :: H.ExportSpecList H.SrcSpanInfo
    }
  deriving (Show)

data PrettyModule =
  PrettyModule
    { name :: String
    , path :: String
    , exportList :: DT.Text
    }
  deriving (Show)

data State =
  State
    { rootDir :: String
    , modulePaths :: [String]
    }
  deriving (Show)

data MetaModule =
  MetaModule
    { name :: String
    , path :: Maybe String
    , specMap :: SpecMap
    }
  deriving (Show)
