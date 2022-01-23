module MExport.Parser
  ( parser
  ) where

import Control.Lens ((^.))
import Control.Monad
import Prelude

import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Language.Haskell.Exts as H
import qualified Language.Haskell.Names.SyntaxUtils as SU

import qualified MExport.Accessor as MA
import qualified MExport.Config as CC
import qualified MExport.Types as LT
import qualified MExport.Utils as U

parser :: CC.Config -> LT.State -> IO (LT.Project LT.Module)
parser _ (LT.State rootDir modulePaths) = do
  modules <- return . HM.elems =<< traverseModules modulePaths
  projectModules <- mapM transform $ filter (DM.isJust . (^. MA.path)) modules
  return $ LT.Project rootDir projectModules

dummySpanInfo :: H.SrcSpanInfo
dummySpanInfo = H.SrcSpanInfo {H.srcInfoSpan = H.SrcSpan "<interactive>" 0 0 0 0, H.srcInfoPoints = []}

transform :: LT.MetaModule -> IO LT.Module
transform (LT.MetaModule name (Just path) specMap) =
  return $ LT.Module name path $ H.ExportSpecList dummySpanInfo $ mapImportToExportDecl <$> HM.elems specMap
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
    addImportSpec :: LT.MetaModule -> H.ImportSpec H.SrcSpanInfo -> LT.MetaModule
    addImportSpec metaModule impSpec = do
      let specMap = metaModule ^. MA.specMap
          idName = getIdentifier impSpec
          exportMap = HM.insertWith getImpPreference idName impSpec specMap
      metaModule {LT._specMap = exportMap}
    parseImpDecl :: HM.HashMap String LT.MetaModule -> H.ImportDecl H.SrcSpanInfo -> HM.HashMap String LT.MetaModule
    parseImpDecl metaModules (H.ImportDecl _ (H.ModuleName _ name) _ _ _ _ _ specList) = do
      case specList of
        Just (H.ImportSpecList _ hiding specs) -> do
          let metaModule = HM.lookupDefault (LT.MetaModule name Nothing HM.empty) name metaModules
          if hiding
            then metaModules
            else HM.insert name (DF.foldl addImportSpec metaModule specs) metaModules
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

getName :: H.Name H.SrcSpanInfo -> String
getName (H.Ident _ name) = name
getName (H.Symbol _ name) = name

getImpPreference :: H.ImportSpec H.SrcSpanInfo -> H.ImportSpec H.SrcSpanInfo -> H.ImportSpec H.SrcSpanInfo
getImpPreference (H.IVar _ _) y@(H.IAbs _ _ _) = y
getImpPreference (H.IVar _ _) y@(H.IThingWith _ _ _) = y
getImpPreference (H.IAbs _ _ _) y@(H.IThingWith _ _ _) = y
getImpPreference x@(H.IThingWith l w cName1) y@(H.IThingWith _ _ cName2) =
  if null cName1
    then x
    else if null cName2
           then y
           else H.IThingWith l w $ DL.nubBy cNameComparator $ cName1 ++ cName2
getImpPreference (H.IVar _ _) y@(H.IThingAll _ _) = y
getImpPreference (H.IAbs _ _ _) y@(H.IThingAll _ _) = y
getImpPreference (H.IThingWith _ _ _) y@(H.IThingAll _ _) = y
getImpPreference x _ = x

cNameComparator :: H.CName H.SrcSpanInfo -> H.CName H.SrcSpanInfo -> Bool
cNameComparator (getCName -> cName1) (getCName -> cName2) = cName1 == cName2

getCName :: H.CName H.SrcSpanInfo -> String
getCName (H.VarName _ name) = getName name
getCName (H.ConName _ name) = getName name

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
