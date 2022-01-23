module MExport.Parser
  ( parser
  ) where

import Control.Monad
import Prelude

import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Language.Haskell.Exts as H
import qualified Language.Haskell.Names.SyntaxUtils as SU

import qualified MExport.Config as CC
import qualified MExport.Types as LT
import qualified MExport.Utils as U

parser :: CC.Config -> LT.State -> IO (LT.Project LT.Module)
parser _ (LT.State rootDir modulePaths) = do
  modules <-
    return . HM.elems =<<
    mapM transform =<< return . HM.filter (\(LT.MetaModule _ path _) -> DM.isJust path) =<< traverseModules modulePaths
  putStrLn $ show modules
  return $ LT.Project rootDir modules

dummySpanInfo :: H.SrcSpanInfo
dummySpanInfo = H.SrcSpanInfo {H.srcInfoSpan = H.SrcSpan "<interactive>" 0 0 0 0, H.srcInfoPoints = []}

transform :: LT.MetaModule -> IO LT.Module
transform (LT.MetaModule name (Just path) specMap) =
  return $ LT.Module name path $ H.ExportSpecList dummySpanInfo $ HM.elems specMap
transform (LT.MetaModule name _ _) = error $ "FilePath not found for module: " ++ name

traverseModules :: [String] -> IO (HM.HashMap String LT.MetaModule)
traverseModules =
  DF.foldlM
    (\metaModules modulePath -> do
       putStrLn $ "Parsing file: " ++ modulePath
       moduleContent <- readFile modulePath
       _module <- parseModuleContent moduleContent U.customExtensions
       let importDecls = SU.getImports _module
           (H.ModuleName _ moduleName) = SU.getModuleName _module
           metaModule =
             DM.maybe
               (LT.MetaModule moduleName (Just modulePath) HM.empty)
               (\(LT.MetaModule name _ specMap) -> LT.MetaModule name (Just modulePath) specMap) $
             HM.lookup moduleName metaModules
           _metaModules = HM.insert moduleName metaModule metaModules
       return $ DF.foldl parseImpDecl _metaModules importDecls)
    HM.empty
  where
    addExportSpec :: LT.MetaModule -> H.ImportSpec H.SrcSpanInfo -> LT.MetaModule
    addExportSpec metaModule impSpec = do
      let specMap = LT.specMap metaModule
          exportSpec = mapImportToExportDecl impSpec
          idName = getIdentifier impSpec
          exportMap = HM.insertWith getPreference idName exportSpec specMap
      metaModule {LT.specMap = exportMap}
    parseImpDecl :: HM.HashMap String LT.MetaModule -> H.ImportDecl H.SrcSpanInfo -> HM.HashMap String LT.MetaModule
    parseImpDecl metaModules (H.ImportDecl _ (H.ModuleName _ name) _ _ _ _ _ specList) = do
      case specList of
        Just (H.ImportSpecList _ hiding specs) -> do
          let metaModule = HM.lookupDefault (LT.MetaModule name Nothing HM.empty) name metaModules
          if hiding
            then metaModules
            else HM.insert name (DF.foldl addExportSpec metaModule specs) metaModules
        _ -> metaModules

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
