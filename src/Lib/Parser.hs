module Lib.Parser
  ( parser
  ) where

import Control.Monad
import Prelude

import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as HM
import qualified Data.IORef as DI
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Language.Haskell.Exts as H
import qualified Language.Haskell.Names.SyntaxUtils as SU
import qualified System.Directory as SD
import qualified System.FilePath as SF

import qualified Lib.Config as CC
import qualified Lib.Types as LT
import qualified Lib.Utils as U

parser :: CC.Config -> String -> IO (HM.HashMap String (H.ExportSpecList H.SrcSpanInfo))
parser _ moduleSrc = do
  let mainSrcDir = SF.takeDirectory moduleSrc
  moduleMapRef <- DI.newIORef (HM.empty :: LT.ModuleMap)
  traverseModules moduleMapRef mainSrcDir moduleSrc
  moduleMap <- DI.readIORef moduleMapRef
  return $ transform <$> moduleMap

dummySpanInfo :: H.SrcSpanInfo
dummySpanInfo = H.SrcSpanInfo {H.srcInfoSpan = H.SrcSpan "<interactive>" 0 0 0 0, H.srcInfoPoints = []}

transform :: HM.HashMap String (H.ExportSpec H.SrcSpanInfo) -> H.ExportSpecList H.SrcSpanInfo
transform specMap = H.ExportSpecList dummySpanInfo (HM.elems specMap)

traverseModules :: DI.IORef LT.ModuleMap -> String -> String -> IO ()
traverseModules moduleMapRef mainSrcDir moduleSrc = do
  moduleContent <- readFile moduleSrc
  _module <- parseModuleContent moduleContent U.customExtensions
  let importDecls = SU.getImports _module
  DF.traverse_
    (\(H.ImportDecl _ (H.ModuleName _ name) _ _ _ _ _ specList) -> do
       moduleMap <- DI.readIORef moduleMapRef
       let impModuleSrc = SF.joinPath [mainSrcDir, ((U.moduleToPath name) <> ".hs") :: SF.FilePath]
           isModuleVisited = DM.isJust $ HM.lookup name moduleMap
       moduleExists <- SD.doesFileExist impModuleSrc -- TODO: Add options to show warning if module doesn't exist
       when (not isModuleVisited && moduleExists) $ traverseModules moduleMapRef mainSrcDir impModuleSrc
       when moduleExists $
         case specList of
           Just (H.ImportSpecList _ hiding specs) -> do
             unless hiding $ DF.traverse_ (addExportSpec name) specs
           _ -> return ())
    importDecls
  where
    addExportSpec :: String -> H.ImportSpec H.SrcSpanInfo -> IO ()
    addExportSpec moduleName impSpec = do
      moduleMap <- DI.readIORef moduleMapRef
      let specMap = DM.fromMaybe HM.empty $ HM.lookup moduleName moduleMap
          exportSpec = mapImportToExportDecl impSpec
          idName = getIdentifier impSpec
      DI.writeIORef moduleMapRef $ HM.insert moduleName (HM.insertWith getPreference idName exportSpec specMap) moduleMap

mapImportToExportDecl :: H.ImportSpec H.SrcSpanInfo -> H.ExportSpec H.SrcSpanInfo
mapImportToExportDecl =
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
getPreference x@(H.EModuleContents _ _) _ = x
getPreference _ y@(H.EModuleContents _ _) = y
getPreference x@(H.EThingWith l w q cName1) y@(H.EThingWith _ _ _ cName2) =
  if null cName1
    then x
    else if null cName2
           then y
           else H.EThingWith l w q $ DL.nub $ cName1 ++ cName2
getPreference _ y@(H.EThingWith _ _ _ _) = y
getPreference (H.EVar _ _) y@(H.EAbs _ _ _) = y
getPreference x _ = x

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
      Left err -> error $ "Exception: " <> show err
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
