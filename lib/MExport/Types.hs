module MExport.Types where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as DT
import qualified GHC.Hs as GHC (GhcPs, IE, LIE)
import qualified SrcLoc as GHC (Located)

type ModuleMap = HM.HashMap String Module

type SpecMap = HM.HashMap String (GHC.IE GHC.GhcPs)

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

data MetaModule =
  MetaModule
    { _name :: String
    , _path :: Maybe String
    , _specMap :: SpecMap
    , _xCoords :: Maybe XCoord
    }

data PrettyModule =
  PrettyModule
    { _name :: String
    , _path :: String
    , _exportList :: DT.Text
    , _exportCoords :: Maybe XCoord
    }
  deriving (Show, Eq)

data State =
  State
    { _rootDir :: String
    , _modulePaths :: [String]
    }
  deriving (Show)

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
