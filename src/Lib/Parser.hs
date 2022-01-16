module Lib.Parser
  ( parser
  ) where

import Control.Monad
import Prelude

import qualified Config.Config as CC
import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as HM
import qualified Data.IORef as DI
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Data.Text as DT
import qualified Data.Text.IO as TIO
import qualified Data.Text.Internal.Search as DTS
import qualified Language.Haskell.Exts as H
import Language.Haskell.Exts.SrcLoc
import qualified Language.Haskell.Interpreter as I
import Language.Haskell.Interpreter (GhcError(..), Interpreter, InterpreterError(..))
import qualified Language.Haskell.Names.SyntaxUtils as SU
import qualified Lib.Types as LT
import qualified System.Directory as SD
import qualified System.FilePath as SF
import qualified Utils.Utils as U

parser :: String -> IO ()
parser moduleSrc = do
  moduleMapRef <- DI.newIORef (HM.empty :: LT.ModuleMap)
  moduleExportRef <- DI.newIORef (HM.empty :: HM.HashMap String H.SrcSpan)
  let mainSrcDir = SF.takeDirectory moduleSrc
  traverseModules moduleMapRef moduleExportRef mainSrcDir moduleSrc
  moduleMap <- DI.readIORef moduleMapRef
  moduleExportMap <- DI.readIORef moduleExportRef
  writeExports ((H.prettyPrint . transform) <$> moduleMap) moduleExportMap mainSrcDir
  return ()

dummySpanInfo :: H.SrcSpanInfo
dummySpanInfo = H.SrcSpanInfo {H.srcInfoSpan = H.SrcSpan "<interactive>" 0 0 0 0, H.srcInfoPoints = []}

transform :: HM.HashMap String (H.ExportSpec H.SrcSpanInfo) -> H.ExportSpecList H.SrcSpanInfo
transform specMap = H.ExportSpecList dummySpanInfo (HM.elems specMap)

traverseModules :: DI.IORef LT.ModuleMap -> DI.IORef (HM.HashMap String H.SrcSpan) -> String -> String -> IO ()
traverseModules moduleMapRef moduleExportRef mainSrcDir moduleSrc
  -- putStrLn $ "Parsing module: " <> moduleSrc
 = do
  moduleContent <- readFile moduleSrc
  _module <- parseModuleContent moduleContent customExtensions
  let importDecls = SU.getImports _module
      _mName@(H.ModuleName _ moduleName) = SU.getModuleName _module
      maybeExportList = SU.getExportSpecList _module
  moduleHead <-
    case _module of
      (H.Module _ (Just moduleHead) _ _ _) -> return moduleHead
      _ -> error "Couldn't find head of module: " moduleName
  let exportSpan = getExportCords moduleHead
  exportSpanMap <- DI.readIORef moduleExportRef
  DI.writeIORef moduleExportRef $ HM.insert moduleName exportSpan exportSpanMap
  -- putStrLn $ "Imports of module: " <> moduleName
  -- putStrLn (show importDecls)
  -- putStrLn $ "Exports of module: " <> moduleName
  -- putStrLn $ show $ SU.getExportSpecList _module
  DF.traverse_
    (\(H.ImportDecl _ (H.ModuleName _ name) _ _ _ _ _ specList) -> do
       let impModuleSrc = SF.joinPath [mainSrcDir, ((U.moduleToPath name) <> ".hs") :: SF.FilePath]
      --  putStrLn $ "Import: " <> impModuleSrc
       moduleMap <- DI.readIORef moduleMapRef
       when (DM.isNothing $ HM.lookup name moduleMap) $ do
         moduleExists <- SD.doesFileExist impModuleSrc -- TODO: Add options to show warning if module doesn't exist
         when moduleExists $ do
           traverseModules moduleMapRef moduleExportRef mainSrcDir impModuleSrc
           case specList of
             Just (H.ImportSpecList _ hiding specs) -> do
               unless hiding $ DF.traverse_ (addExportSpec moduleMapRef name) specs
             _ -> return ())
    importDecls
  return ()
  where
    addExportSpec :: DI.IORef LT.ModuleMap -> String -> H.ImportSpec H.SrcSpanInfo -> IO ()
    addExportSpec moduleMapRef moduleName impSpec = do
      moduleMap <- DI.readIORef moduleMapRef
      let specMap = DM.fromMaybe HM.empty $ HM.lookup moduleName moduleMap
          exportSpec = mapImportToExportDecl moduleName impSpec
          idName = getIdentifier impSpec
      DI.writeIORef moduleMapRef $ HM.insert moduleName (HM.insertWith getPreference idName exportSpec specMap) moduleMap

getExportCords :: H.ModuleHead H.SrcSpanInfo -> H.SrcSpan
getExportCords (H.ModuleHead (H.SrcSpanInfo (H.SrcSpan _ _ _ r2 c2) _) (H.ModuleName (H.SrcSpanInfo (H.SrcSpan _ _ _ r1 c1) _) _) _ _) =
  H.SrcSpan "<interactive>" r1 c1 r2 c2

mapImportToExportDecl :: String -> H.ImportSpec H.SrcSpanInfo -> H.ExportSpec H.SrcSpanInfo
mapImportToExportDecl moduleName =
  \case
    (H.IVar l name) -> H.EVar l (H.UnQual l name)
    (H.IAbs l nameSpace name) -> H.EAbs l nameSpace (H.UnQual l name)
    (H.IThingWith l name cName) -> H.EThingWith l (H.NoWildcard l) (H.UnQual l name) cName
    (H.IThingAll l name) -> H.EThingWith l (H.EWildcard l 0) (H.UnQual l name) []

getIdentifier :: H.ImportSpec H.SrcSpanInfo -> String
getIdentifier =
  \case
    (H.IVar _ name) -> getName name
    (H.IAbs _ _ name) -> getName name
    (H.IThingWith _ name _) -> getName name
    (H.IThingAll _ name) -> getName name
  where
    getName :: H.Name H.SrcSpanInfo -> String
    getName (H.Ident _ name) = name
    getName (H.Symbol _ name) = name

getPreference :: H.ExportSpec H.SrcSpanInfo -> H.ExportSpec H.SrcSpanInfo -> H.ExportSpec H.SrcSpanInfo
getPreference x@(H.EModuleContents _ _) y = x
getPreference x y@(H.EModuleContents _ _) = y
getPreference x@(H.EThingWith l w q cName1) y@(H.EThingWith _ _ _ cName2) =
  if null cName1
    then x
    else if null cName2
           then y
           else H.EThingWith l w q $ DL.nub $ cName1 ++ cName2
getPreference x y@(H.EThingWith _ _ _ _) = y
getPreference x@(H.EVar _ _) y@(H.EAbs _ _ _) = y
getPreference x y = x

{-
  Parses the module content provided in the first param @moduleContent@
  using the custom extensions provided via @customExtensions@.
  This will return the module
-}
parseModuleContent :: String -> [H.Extension] -> IO (H.Module H.SrcSpanInfo)
parseModuleContent moduleContent customExtensions = do
  let maybeModuleInfo = H.readExtensions moduleContent
      extensions =
        case maybeModuleInfo of
          Just (_, exts) -> exts
          _ -> []
  let parseResult = H.parseModuleWithMode (mode (extensions ++ customExtensions)) moduleContent
      parsedModuleDeclE =
        case parseResult of
          H.ParseOk parsed -> Right parsed
          H.ParseFailed _ errorReason -> Left errorReason
  _module <-
    case parsedModuleDeclE of
      Right _module -> return _module
      _ -> error "XmlPage/XmlHybrid module types not supported"
  return _module

{-
  This function provides the parsing mode for a module.
-}
mode :: [H.Extension] -> H.ParseMode
mode extensions = H.defaultParseMode {H.extensions = allExtensions ++ extensions, H.fixities = Nothing}
  where
    allExtensions = filter isDisabledExtension H.knownExtensions
    isDisabledExtension (H.DisableExtension _) = False
    isDisabledExtension _ = True

-- TODO: Add options to provide extensions
customExtensions :: [H.Extension]
customExtensions = map H.EnableExtension [H.TypeApplications]

writeExports :: HM.HashMap String String -> HM.HashMap String H.SrcSpan -> String -> IO ()
writeExports moduleMap exportSpanMap mainSrcDir = do
  let modules = HM.keys moduleMap
  forM_
    modules
    (\moduleName -> do
       let modulePath = SF.joinPath [mainSrcDir, ((U.moduleToPath moduleName) <> ".hs") :: SF.FilePath]
       moduleContent <- TIO.readFile modulePath
       let mNameIndex = (length moduleName) + (DM.fromMaybe 7 $ U.findFirstInText (DT.pack moduleName) moduleContent)
           mWhereIndex = (DM.fromMaybe ((length moduleName) + 8) $ U.findFirstInText "where" moduleContent)
           moduleStart = DT.take mNameIndex moduleContent
           moduleRest = DT.drop (mWhereIndex + 5) moduleContent
           moduleExport = DT.pack $ DM.fromMaybe ("" :: String) $ HM.lookup moduleName moduleMap
       TIO.writeFile modulePath (moduleStart <> " " <> moduleExport <> " where" <> moduleRest))
