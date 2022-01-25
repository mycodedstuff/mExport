{-# OPTIONS -Wno-missing-fields #-}

module MExport.Ghc.Parser where

import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import qualified Data.Maybe as DM
import Prelude

import qualified Bag as GHC (bagToList)
import DynFlags (FileSettings(..), Settings(..), defaultDynFlags)
import qualified DynFlags as GHC
import qualified FastString as GHC (mkFastString)
import GHC.Fingerprint (fingerprint0)
import qualified GHC.Hs as GHC (GhcPs, HsModule, hsmodImports, hsmodName)
import qualified GHC.Hs.ImpExp as GHC
import qualified GHC.LanguageExtensions.Type as GHC (Extension(..))
import GHC.Platform
import GHC.Version (cProjectVersion)
import GhcNameVersion (GhcNameVersion(..))
import qualified HeaderInfo as GHC (getOptions)
import qualified HscTypes as GHC (handleSourceError)
import qualified Lexer as GHC (ParseResult(..), getErrorMessages, mkPState, unP)
import qualified Module as GHC (moduleNameString)
import qualified Outputable as GHC (OutputableBndr, ppr, showPpr, showSDocUnsafe)
import qualified Panic as GHC (handleGhcException)
import qualified Parser as GHC
import PlatformConstants (PlatformConstants(..))
import qualified SrcLoc as GHC (GenLocated(..), Located, mkRealSrcLoc, noSrcSpan, unLoc)
import qualified StringBuffer as GHC (stringToStringBuffer)
import qualified ToolSettings as GHC (ToolSettings(..))

import qualified MExport.Config as MC
import qualified MExport.Types as MT

data Module =
  Module
    { _name :: String
    , _modulePath :: String
    , _exportSpecs :: GHC.Located [GHC.LIE GHC.GhcPs]
    }
  -- deriving (Show)

data MetaModule =
  MetaModule
    { _name :: String
    , _path :: Maybe String
    , _specMap :: HM.HashMap String (GHC.IE GHC.GhcPs)
    }
  -- deriving (Show)

parser :: MC.Config -> MT.State -> IO (MT.Project Module)
parser _ (MT.State rootDir modulePaths) = do
  metaModules <- return . HM.elems =<< traverseModules modulePaths
  projectModules <- mapM transform $ filter (DM.isJust . _path) metaModules
  return $ MT.Project rootDir projectModules

transform :: MetaModule -> IO Module
transform (MetaModule name (Just path) specMap) =
  return $ Module name path $ GHC.L GHC.noSrcSpan $ (GHC.L GHC.noSrcSpan) <$> (sortIEList $ HM.elems specMap)
transform (MetaModule name _ _) = error $ "FilePath not found for module: " ++ name

sortIEList :: [GHC.IE GHC.GhcPs] -> [GHC.IE GHC.GhcPs]
sortIEList = DL.sortOn (GHC.showSDocUnsafe . GHC.ppr)

traverseModules :: [String] -> IO (HM.HashMap String MetaModule)
traverseModules =
  DF.foldlM
    (\metaModules modulePath -> do
       putStrLn $ "Parsing file: " ++ modulePath
       moduleContent <- readFile modulePath
       (dynFlags, _module) <- parseModuleContent modulePath moduleContent customExtensions
       let imports = GHC.unLoc <$> GHC.hsmodImports _module
           moduleName =
             maybe "<interactive>" (\(GHC.L _ _moduleName) -> GHC.moduleNameString _moduleName) $ GHC.hsmodName _module
           metaModule =
             DM.maybe
               (MetaModule moduleName (Just modulePath) HM.empty)
               (\(MetaModule name _ specMap) -> MetaModule name (Just modulePath) specMap) $
             HM.lookup moduleName metaModules
           _metaModules = HM.insert moduleName metaModule metaModules
       return $ DF.foldl (parseImpDecl dynFlags) _metaModules imports)
    HM.empty
  where
    addImportSpec :: GHC.DynFlags -> MetaModule -> GHC.LIE GHC.GhcPs -> MetaModule
    addImportSpec dynFlags metaModule (GHC.unLoc -> impSpec) = do
      let specMap = _specMap metaModule
          idName = getIdentifier dynFlags impSpec
          exportMap = HM.insertWith (getImpPreference dynFlags) idName impSpec specMap
      metaModule {_specMap = exportMap}
    parseImpDecl :: GHC.DynFlags -> HM.HashMap String MetaModule -> GHC.ImportDecl GHC.GhcPs -> HM.HashMap String MetaModule
    parseImpDecl dynFlags metaModules importDecl = do
      let (GHC.ImportDecl _ _ mName _ _ _ _ _ _ specList) = importDecl
          name = GHC.moduleNameString $ GHC.unLoc mName
      case specList of
        Just (hiding, GHC.unLoc -> specs) -> do
          let metaModule = HM.lookupDefault (MetaModule name Nothing HM.empty) name metaModules
          if hiding
            then metaModules
            else HM.insert name (DF.foldl (addImportSpec dynFlags) metaModule specs) metaModules
        _ -> metaModules

getIdentifier :: GHC.DynFlags -> GHC.IE GHC.GhcPs -> String
getIdentifier dynFlags =
  \case
    (GHC.IEVar _ name) -> GHC.showPpr dynFlags $ GHC.unLoc name
    (GHC.IEThingAbs _ name) -> GHC.showPpr dynFlags $ GHC.unLoc name
    (GHC.IEThingWith _ name _ _ _) -> GHC.showPpr dynFlags $ GHC.unLoc name
    (GHC.IEThingAll _ name) -> GHC.showPpr dynFlags $ GHC.unLoc name
    (GHC.IEModuleContents _ name) -> GHC.moduleNameString $ GHC.unLoc name
    x@(GHC.IEGroup _ _ _) -> GHC.showPpr dynFlags x
    (GHC.IEDoc _ doc) -> GHC.showPpr dynFlags doc
    (GHC.IEDocNamed _ name) -> name
    (GHC.XIE x) -> GHC.showPpr dynFlags x

getImpPreference :: GHC.DynFlags -> GHC.IE GHC.GhcPs -> GHC.IE GHC.GhcPs -> GHC.IE GHC.GhcPs
getImpPreference _ (GHC.IEVar _ _) y@(GHC.IEThingAbs _ _) = y
getImpPreference _ (GHC.IEVar _ _) y@(GHC.IEThingWith _ _ _ _ _) = y
getImpPreference _ (GHC.IEThingAbs _ _) y@(GHC.IEThingWith _ _ _ _ _) = y
getImpPreference dynFlags x@(GHC.IEThingWith l name w cName1 fld) y@(GHC.IEThingWith _ _ _ cName2 _) =
  if null cName1
    then x
    else if null cName2
           then y
           else GHC.IEThingWith l name w (mergeNames dynFlags cName1 cName2) fld
getImpPreference _ (GHC.IEVar _ _) y@(GHC.IEThingAll _ _) = y
getImpPreference _ (GHC.IEThingAbs _ _) y@(GHC.IEThingAll _ _) = y
getImpPreference _ (GHC.IEThingWith _ _ _ _ _) y@(GHC.IEThingAll _ _) = y
getImpPreference _ x _ = x

mergeNames :: GHC.OutputableBndr a => GHC.DynFlags -> [GHC.LIEWrappedName a] -> [GHC.LIEWrappedName a] -> [GHC.LIEWrappedName a]
mergeNames dynFlags name1 name2 =
  DL.sortOn (GHC.showSDocUnsafe . GHC.ppr) . DL.nubBy (nameComparator dynFlags) $ name1 ++ name2

nameComparator :: GHC.OutputableBndr a => GHC.DynFlags -> GHC.LIEWrappedName a -> GHC.LIEWrappedName a -> Bool
nameComparator dynFlags name1 name2 = (toString name1) == (toString name2)
  where
    toString :: GHC.OutputableBndr a => GHC.LIEWrappedName a -> String
    toString = GHC.showPpr dynFlags . GHC.unLoc

parseModuleContent :: String -> String -> [GHC.Extension] -> IO (GHC.DynFlags, GHC.HsModule GHC.GhcPs)
parseModuleContent modulePath moduleContent customExts = do
  dynFlags <- parsePragmasIntoDynFlags baseDynFlags customExts modulePath moduleContent
  case dynFlags of
    Right flags -> do
      let result = runParser flags modulePath moduleContent
      case result of
        GHC.POk _ (GHC.L _ m) -> return (flags, m)
        GHC.PFailed failureState ->
          error $
          "Exception: " ++
          (DL.foldl (\msg err -> msg ++ ", " ++ show err) "" (GHC.bagToList $ GHC.getErrorMessages failureState flags)) -- TODO: Improve error reporting: https://github.com/digital-asset/ghc-lib/blob/master/examples/mini-hlint/src/Main.hs#L300
    Left err -> error $ "Exception while getting dynFlags: " ++ show err

runParser :: GHC.DynFlags -> String -> String -> GHC.ParseResult (GHC.Located (GHC.HsModule GHC.GhcPs))
runParser flags filePath str =
  let filename = GHC.mkFastString filePath
      parseState = GHC.mkPState flags (GHC.stringToStringBuffer str) (GHC.mkRealSrcLoc filename 1 1)
   in GHC.unP GHC.parseModule parseState

parsePragmasIntoDynFlags :: GHC.DynFlags -> [GHC.Extension] -> FilePath -> String -> IO (Either String GHC.DynFlags)
parsePragmasIntoDynFlags flags customExts filepath str =
  catchErrors $ do
    let opts = GHC.getOptions flags (GHC.stringToStringBuffer str) filepath
        flags' = foldl (\fl ex -> GHC.xopt_set fl ex) flags customExts
    (parsedFlags, _invalidFlags, _warnings) <- GHC.parseDynamicFilePragma flags' opts
    return $ Right $ parsedFlags `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
  where
    catchErrors act = GHC.handleGhcException reportErr (GHC.handleSourceError reportErr act)
    reportErr e = return $ Left (show e)

baseDynFlags :: GHC.DynFlags
baseDynFlags = defaultDynFlags fakeSettings llvmConfig
  where
    fakeSettings =
      GHC.Settings
        { sGhcNameVersion = GhcNameVersion "mexport" cProjectVersion
        , sFileSettings = FileSettings {}
        , sToolSettings = GHC.ToolSettings {toolSettings_opt_P_fingerprint = fingerprint0, toolSettings_pgm_F = ""}
        , sPlatformConstants = PlatformConstants {pc_DYNAMIC_BY_DEFAULT = False, pc_WORD_SIZE = 8}
        , sTargetPlatform =
            Platform
              { platformMini = PlatformMini {platformMini_arch = ArchUnknown, platformMini_os = OSUnknown}
              , platformWordSize = PW8
              , platformUnregisterised = True
              , platformHasGnuNonexecStack = False
              , platformHasIdentDirective = False
              , platformHasSubsectionsViaSymbols = False
              , platformIsCrossCompiling = False
              }
        , sPlatformMisc = PlatformMisc {}
        , sRawSettings = []
        }
    llvmConfig = GHC.LlvmConfig [] []

customExtensions :: [GHC.Extension]
customExtensions =
  [ GHC.BangPatterns
  , GHC.ConstraintKinds
  , GHC.DataKinds
  , GHC.DefaultSignatures
  , GHC.DeriveFunctor
  , GHC.DeriveGeneric
  , GHC.DuplicateRecordFields
  , GHC.ExplicitNamespaces
  , GHC.FlexibleContexts
  , GHC.FlexibleInstances
  , GHC.FunctionalDependencies
  , GHC.GADTs
  , GHC.LambdaCase
  , GHC.MultiParamTypeClasses
  , GHC.MultiWayIf
  , GHC.OverloadedStrings
  , GHC.PatternSynonyms
  , GHC.PolyKinds
  , GHC.RankNTypes
  , GHC.RecordWildCards
  , GHC.ScopedTypeVariables
  , GHC.TupleSections
  , GHC.TypeApplications
  , GHC.TypeFamilies
  , GHC.TypeOperators
  , GHC.ViewPatterns
  , GHC.BlockArguments
  ]