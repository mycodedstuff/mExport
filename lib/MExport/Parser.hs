{-# OPTIONS -Wno-missing-fields #-}

module MExport.Parser where

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad
import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified System.Directory as SD
import qualified System.FilePath as SF

import qualified ApiAnnotation as GHC
import qualified Bag as GHC (bagToList)
import DynFlags (FileSettings(..), Settings(..), defaultDynFlags)
import qualified DynFlags as GHC
import qualified FastString as GHC (mkFastString)
import GHC.Fingerprint (fingerprint0)
import qualified GHC.Hs as GHC (GhcPs, HsModule, hsmodImports, hsmodName)
import qualified GHC.Hs.Extension as GHC (noExtField)
import qualified GHC.Hs.ImpExp as GHC
import GHC.Platform
import GHC.Version (cProjectVersion)
import GhcNameVersion (GhcNameVersion(..))
import qualified HeaderInfo as GHC (getOptions)
import qualified HscTypes as GHC (handleSourceError)
import qualified Lexer as GHC (PState, ParseResult(..), annotations, getErrorMessages, mkPState, unP)
import qualified Module as GHC (moduleNameString)
import qualified Outputable as GHC (OutputableBndr, ppr, showPpr, showSDocUnsafe)
import qualified Panic as GHC (handleGhcException)
import qualified Parser as GHC
import PlatformConstants (PlatformConstants(..))
import qualified SrcLoc as GHC
  ( GenLocated(..)
  , Located
  , SrcSpan(..)
  , getLoc
  , mkRealSrcLoc
  , noSrcSpan
  , realSrcSpanEnd
  , srcLocCol
  , srcLocLine
  , unLoc
  )
import qualified StringBuffer as GHC (stringToStringBuffer)
import qualified ToolSettings as GHC (ToolSettings(..))

import qualified MExport.Accessor as MA
import qualified MExport.Config as MC
import qualified MExport.Types as MT
import qualified MExport.Utils.Parser as UP
import qualified MExport.Utils.Utils as MU

parser :: MC.Config -> MT.ProjectInfo MT.ModuleMetadata -> IO (MT.Project MT.Module)
parser config (MT.ProjectInfo projectName projectDir packages) = do
  putStrLn $ "Parsing project " ++ projectName
  let customExtensions = MC.extensions config
      maybeDumpDir = MC.dumpDir config
  metaPkgs <- transformPackages maybeDumpDir customExtensions packages
  pkgMetaMap <-
    DF.foldlM
      (\pkgMap pkg -> do
         DF.foldlM
           (\pkgMap' moduleInfo -> do
              let pkgName = pkg ^. MA.pkgName
                  moduleImports = moduleInfo ^. MA.imports
                  moduleName = moduleInfo ^. MA.name
                  infoMap =
                    HM.insertWith (\_ m -> m) moduleName moduleInfo $ snd $ HM.lookupDefault (pkg, HM.empty) pkgName pkgMap'
                  pkgMInfoMap = HM.insert pkgName (pkg, infoMap) pkgMap'
              putStrLn $ "Processing imports of " ++ moduleName ++ " of " ++ pkgName
              DF.foldlM (parseImpDecl moduleName metaPkgs pkg) pkgMInfoMap moduleImports)
           pkgMap
           (pkg ^. MA.pkgModules))
      HM.empty
      metaPkgs
  modules <- mapM (transform' config) $ HM.foldl (\moduleArr (_, metaMap) -> moduleArr ++ (HM.elems metaMap)) [] pkgMetaMap
  return $ MT.Project projectDir modules
  where
    addImportSpec :: MT.ModuleInfo -> GHC.LIE GHC.GhcPs -> MT.ModuleInfo
    addImportSpec moduleInfo (GHC.unLoc -> impSpec) = do
      let specMap = moduleInfo ^. MA.specMap
          dynFlags = moduleInfo ^. MA.dynFlags
          idName = getIdentifier dynFlags impSpec
          exportMap = HM.insertWith (getImpPreference dynFlags) idName impSpec specMap
      moduleInfo {MT._specMap = exportMap}
    parseImpDecl ::
         String
      -> [MT.Package MT.ModuleInfo]
      -> MT.Package MT.ModuleInfo
      -> MT.PkgMetaMap
      -> GHC.ImportDecl GHC.GhcPs
      -> IO MT.PkgMetaMap
    parseImpDecl moduleName packages package pkgMetaMap importDecl = do
      case importDecl of
        GHC.ImportDecl _ _ mName _ _ _ _ _ _ specList -> do
          let name = GHC.moduleNameString $ GHC.unLoc mName
          case specList of
            Just (hiding, GHC.unLoc -> specs) -> do
              let mTuple = findMetadataFromName name package packages
              case mTuple of
                Just (pkg, moduleInfo) ->
                  if hiding
                    then return pkgMetaMap
                    else do
                      let pkgName = pkg ^. MA.pkgName
                          infoMap = snd $ HM.lookupDefault (pkg, HM.empty) pkgName pkgMetaMap
                          moduleInfo' = DF.foldl addImportSpec (HM.lookupDefault moduleInfo name infoMap) specs
                          infoMap' = HM.insert name moduleInfo' infoMap
                      return $ HM.insert pkgName (pkg, infoMap') pkgMetaMap
                Nothing -> return pkgMetaMap
            _ -> return pkgMetaMap
        _ -> return pkgMetaMap

transformPackages :: Maybe String -> [String] -> [MT.Package MT.ModuleMetadata] -> IO [MT.Package MT.ModuleInfo]
transformPackages maybeDumpDir customExtensions packages =
  mapM
    (\pkg -> do
       let pkgName = pkg ^. MA.pkgName
           pkgDumpDir = pkg ^. MA.dumpDir
       moduleInfos <- mapM (parseModule pkgName pkgDumpDir) $ pkg ^. MA.pkgModules
       return $ pkg {MT._pkgModules = moduleInfos})
    packages
  where
    parseModule :: String -> Maybe String -> MT.ModuleMetadata -> IO MT.ModuleInfo
    parseModule pkgName pkgDumpDir MT.ModuleMetadata {..} = do
      putStrLn $ "Parsing module " ++ _name ++ " ~> " ++ _path
      moduleContent <- readFile _path
      (dynFlags, pState, _module) <- parseModuleContent _path moduleContent customExtensions
      let coords = getXCoord pState _module
          mDumpDir = maybeDumpDir <|> pkgDumpDir
      mMinimalImports <-
        case mDumpDir of
          Just dumpDir -> findMinimalImport dumpDir _name
          Nothing -> return Nothing
      let moduleImports =
            case mMinimalImports of
              Just minimalImports -> minimalImports
              Nothing -> GHC.unLoc <$> GHC.hsmodImports _module
      return $ MT.ModuleInfo _name _path HM.empty coords _module moduleImports dynFlags

transform' :: MC.Config -> MT.ModuleInfo -> IO MT.Module
transform' config (MT.ModuleInfo name path specMap coords _module _ _) = do
  exportList <- mapM (reduceIE config (Just _module)) $ sortIEList $ HM.elems specMap
  return $ MT.Module name path coords $ GHC.L GHC.noSrcSpan $ (GHC.L GHC.noSrcSpan) <$> exportList

printPackageDebug :: MT.PkgMetaMap -> IO MT.PkgMetaMap
printPackageDebug pkgMInfoMap = do
  putStrLn "Printing Details: -----"
  DF.forM_
    (HM.elems pkgMInfoMap)
    (\(pkg, metaMap) -> do
       putStrLn $ "Package: " ++ (pkg ^. MA.pkgName)
       let modules = HM.elems metaMap
       DF.forM_
         modules
         (\moduleInfo' -> do
            putStrLn $ "Module: " ++ (moduleInfo' ^. MA.name)
            putStrLn $ "specMap: " ++ show (HM.map (getIdentifier (moduleInfo' ^. MA.dynFlags)) $ moduleInfo' ^. MA.specMap)))
  return pkgMInfoMap

sortIEList :: [GHC.IE GHC.GhcPs] -> [GHC.IE GHC.GhcPs]
sortIEList = DL.sortOn (GHC.showSDocUnsafe . GHC.ppr)

reduceIE :: MC.Config -> Maybe (GHC.HsModule GHC.GhcPs) -> GHC.IE GHC.GhcPs -> IO (GHC.IE GHC.GhcPs)
reduceIE config (Just _module) ie@(GHC.IEThingWith _ name _ cNames _) = do
  let typeName = GHC.showSDocUnsafe $ GHC.ppr name
      exportedCount = 1 + DL.length cNames -- Counting the exported fields and constructors plus the data type
      percentage = MC.collapseAfter $ MC.codeStyle config
  exportableCount <- return . DM.fromMaybe 0 =<< UP.getExportableCount _module typeName
  return $ collapseDecision exportableCount exportedCount percentage
  where
    collapseDecision :: Int -> Int -> Int -> GHC.IE GHC.GhcPs
    collapseDecision exportableCount exportedCount percentage
      | exportedCount * 100 >= percentage * exportableCount = GHC.IEThingAll GHC.noExtField name
      | otherwise = ie
reduceIE _ _ x = return x

findMinimalImport :: String -> String -> IO (Maybe [GHC.ImportDecl GHC.GhcPs])
findMinimalImport dumpDir moduleName = do
  let importFile = dumpDir SF.</> moduleName <> ".imports"
  fileExist <- SD.doesFileExist importFile
  if fileExist
    then do
      putStrLn $ "Using import dump file " ++ importFile
      importContent <- readFile importFile
      (_, _, _module) <- parseModuleContent importFile importContent []
      return $ Just $ GHC.unLoc <$> GHC.hsmodImports _module
    else putStrLn ("Import dump file not available: " ++ importFile) *> return Nothing

findMetadataFromName ::
     String -> MT.Package MT.ModuleInfo -> [MT.Package MT.ModuleInfo] -> Maybe (MT.Package MT.ModuleInfo, MT.ModuleInfo)
findMetadataFromName moduleName package packages =
  let mMetadata = MU.headMaybe $ filter (\MT.ModuleInfo {..} -> _name == moduleName) $ package ^. MA.pkgModules
   in case mMetadata of
        Just metadata -> Just (package, metadata)
        Nothing ->
          let deps = package ^. MA.dependencies
              depPkgs = filter (\pkg -> (pkg ^. MA.pkgName) `elem` deps) packages
           in foldl
                (\result pkg ->
                   case result of
                     Just val -> Just val
                     Nothing -> findMetadataFromName moduleName pkg packages)
                Nothing
                depPkgs

-- Takes SrcSpan of ModuleName and where keyword and returns XCoord
getXCoord :: GHC.PState -> GHC.HsModule GHC.GhcPs -> DM.Maybe MT.XCoord
getXCoord pState _module = do
  let srcSpan = DM.fromMaybe GHC.noSrcSpan $ GHC.getLoc <$> GHC.hsmodName _module
  (_, srcSpanArr) <- MU.headMaybe . DL.filter ((== GHC.AnnWhere) . snd . fst) $ GHC.annotations pState
  annSrcSpan <- MU.headMaybe srcSpanArr
  case (srcSpan, annSrcSpan) of
    (GHC.RealSrcSpan hSpan, GHC.RealSrcSpan wSpan) -> do
      let startSpan = GHC.realSrcSpanEnd hSpan
          endSpan = GHC.realSrcSpanEnd wSpan
      Just $ MT.XCoord (GHC.srcLocLine startSpan) (GHC.srcLocCol startSpan) (GHC.srcLocLine endSpan) (GHC.srcLocCol endSpan)
    (_, _) -> Nothing

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

mergeNames ::
     GHC.OutputableBndr a => GHC.DynFlags -> [GHC.LIEWrappedName a] -> [GHC.LIEWrappedName a] -> [GHC.LIEWrappedName a]
mergeNames dynFlags name1 name2 =
  DL.sortOn (GHC.showSDocUnsafe . GHC.ppr) . DL.nubBy (nameComparator dynFlags) $ name1 ++ name2

nameComparator :: GHC.OutputableBndr a => GHC.DynFlags -> GHC.LIEWrappedName a -> GHC.LIEWrappedName a -> Bool
nameComparator dynFlags name1 name2 = (toString name1) == (toString name2)
  where
    toString :: GHC.OutputableBndr a => GHC.LIEWrappedName a -> String
    toString = GHC.showPpr dynFlags . GHC.unLoc

parseModuleContent :: String -> String -> [String] -> IO (GHC.DynFlags, GHC.PState, GHC.HsModule GHC.GhcPs)
parseModuleContent modulePath moduleContent customExts = do
  dynFlags <- parsePragmasIntoDynFlags baseDynFlags customExts modulePath moduleContent
  case dynFlags of
    Right flags -> do
      let result = runParser flags modulePath moduleContent
      case result of
        GHC.POk pState (GHC.L _ m) -> return (flags, pState, m)
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

parsePragmasIntoDynFlags :: GHC.DynFlags -> [String] -> FilePath -> String -> IO (Either String GHC.DynFlags)
parsePragmasIntoDynFlags flags customExts filepath str =
  catchErrors $ do
    let opts = GHC.getOptions flags (GHC.stringToStringBuffer str) filepath
        extOpts = map (\ext -> GHC.L GHC.noSrcSpan $ "-X" <> ext) customExts
    (parsedFlags, _invalidFlags, _warnings) <- GHC.parseDynamicFilePragma flags (opts <> extOpts)
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
