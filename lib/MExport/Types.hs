module MExport.Types where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as DT
import qualified DynFlags as GHC (DynFlags)
import qualified GHC.Hs as GHC (GhcPs, HsModule, IE, ImportDecl, LIE)
import qualified SrcLoc as GHC (Located)

type SpecMap = HM.HashMap String (GHC.IE GHC.GhcPs)

type PkgMetaMap = HM.HashMap String (Package ModuleInfo, HM.HashMap String ModuleInfo)

data Project a =
  Project
    { _rootDir :: String
    , _modules :: [a]
    }
  deriving (Show, Eq)

data Module =
  Module
    { _name :: String
    , _modulePath :: String
    , _exportCoords :: Maybe XCoord
    , _exportSpecs :: GHC.Located [GHC.LIE GHC.GhcPs]
    }

data ModuleInfo =
  ModuleInfo
    { _name :: String
    , _path :: String
    , _specMap :: SpecMap
    , _xCoords :: Maybe XCoord
    , _parsedModule :: GHC.HsModule GHC.GhcPs
    , _imports :: [GHC.ImportDecl GHC.GhcPs]
    , _dynFlags :: GHC.DynFlags
    }

data Package m =
  Package
    { _pkgName :: String
    , _pkgModules :: [m]
    , _srcDir :: [String]
    , _dependencies :: [String]
    , _pkgType :: PackageType
    , _dumpDir :: Maybe String
    }
  deriving (Show)

data PackageType
  = Executable
  | Library
  | SubLibrary
  | TestSuite
  | Benchmark
  deriving (Show, Eq)

data ModuleMetadata =
  ModuleMetadata
    { _name :: String
    , _path :: String
    }
  deriving (Show)

data ProjectInfo m =
  ProjectInfo
    { _projectName :: String
    , _path :: String
    , _packages :: [Package m]
    }
  deriving (Show)

data PrettyModule =
  PrettyModule
    { _name :: String
    , _path :: String
    , _exportList :: DT.Text
    , _exportCoords :: Maybe XCoord
    }
  deriving (Show, Eq)

-- PrettyState Brackets Spaces
data PrettyState =
  PrettyState DT.Text Int Int
  deriving (Show)

{- XCoord contains the line and column info of where Module name and where keyword ends
   XCoord l1 c1 l2 c2
   l1 -> Line where module name is present
   c1 -> Column where module name ends
   l2 -> Line at which where keyword is present
   c2 -> Column at which where keyword ends
-}
data XCoord =
  XCoord Int Int Int Int
  deriving (Show, Eq)

data Builder = STACK | CABAL
