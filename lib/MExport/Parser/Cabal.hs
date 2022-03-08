module MExport.Parser.Cabal where

import Control.Lens
import Control.Monad
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Data.Version as DV
import qualified System.Directory as SD
import qualified System.Exit as SE
import qualified System.FilePath as SF
import qualified System.Process as SP
import qualified Text.PrettyPrint as PP

import qualified MExport.Accessor as MA
import qualified MExport.Types as MT
import qualified MExport.Utils.Utils as MU

import qualified Distribution.ModuleName as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.System as Cabal
import qualified Distribution.Types.Dependency as Cabal
import qualified Distribution.Types.LibraryName as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Distribution.Verbosity as Cabal

parseCabalFile :: String -> IO (MT.ProjectInfo MT.ModuleMetadata)
parseCabalFile filePath = do
  gPkg <- Cabal.readGenericPackageDescription Cabal.silent filePath
  let projectIdentifier = Cabal.package $ Cabal.packageDescription gPkg
      projectName = Cabal.unPackageName $ Cabal.pkgName projectIdentifier
      projectVersion = PP.render $ Cabal.pretty $ Cabal.pkgVersion projectIdentifier
      projectPath = SF.takeDirectory filePath
  builder <- getBuilderType projectPath
  packages <- mkPackages projectName projectVersion projectPath builder gPkg
  return $ MT.ProjectInfo projectName projectPath packages

getBuilderType :: String -> IO MT.Builder
getBuilderType projectPath = do
  stackExist <- SD.doesFileExist $ SF.joinPath [projectPath, "stack.yaml"]
  if stackExist
    then return MT.STACK
    else return MT.CABAL

mkPackages ::
     String -> String -> String -> MT.Builder -> Cabal.GenericPackageDescription -> IO [MT.Package MT.ModuleMetadata]
mkPackages projectName projectVersion projectPath builder pkgDesc = do
  mLibraryPkg <-
    mapM (mkPackageFromLibrary projectName projectVersion projectPath builder MT.Library) $
    Cabal.condTreeData <$> Cabal.condLibrary pkgDesc
  executablePkgs <-
    mapM ((mkPackageFromExecutable projectName projectVersion projectPath builder) . Cabal.condTreeData . snd) $
    Cabal.condExecutables pkgDesc
  let depPackages =
        case mLibraryPkg of
          Just lib -> [lib]
          Nothing -> []
  return $
    depPackages ++
    (map
       (\pkg ->
          let deps = pkg ^. MA.dependencies
           in set MA.dependencies (removeExternalDependencies depPackages deps) pkg)
       executablePkgs)
  where
    isDependency :: [MT.Package MT.ModuleMetadata] -> String -> Bool
    isDependency packages dep =
      DM.isJust $
      MU.headMaybe $ filter (\MT.Package {..} -> (_pkgName == "library" && dep == projectName) || dep == _pkgName) packages
    removeExternalDependencies :: [MT.Package MT.ModuleMetadata] -> [String] -> [String]
    removeExternalDependencies packages dependencies =
      foldl
        (\deps dep ->
           if isDependency packages dep
             then if dep == projectName
                    then "library" : deps
                    else dep : deps
             else deps)
        []
        dependencies

parseBuildInfo :: String -> Cabal.BuildInfo -> IO ([String], [MT.ModuleMetadata], [String])
parseBuildInfo projectPath buildInfo = do
  let otherModules = Cabal.otherModules buildInfo
      hsSrcDirs = Cabal.hsSourceDirs buildInfo
      dependencies = Cabal.unPackageName . Cabal.depPkgName <$> Cabal.targetBuildDepends buildInfo
  moduleMetadata <- pairModuleWithPath projectPath hsSrcDirs otherModules
  return (hsSrcDirs, moduleMetadata, dependencies)

mkPackageFromLibrary ::
     String -> String -> String -> MT.Builder -> MT.PackageType -> Cabal.Library -> IO (MT.Package MT.ModuleMetadata)
mkPackageFromLibrary projectName projectVersion projectPath builder pkgType library = do
  let exposedModules = Cabal.exposedModules library
      libName = DM.fromMaybe "library" $ Cabal.unUnqualComponentName <$> Cabal.libraryNameString (Cabal.libName library)
  dumpDir <- getDumpDir projectName projectVersion projectPath libName pkgType builder
  (hsSrcDirs, moduleNames, dependencies) <- parseBuildInfo projectPath $ Cabal.libBuildInfo library
  exModules <- pairModuleWithPath projectPath hsSrcDirs exposedModules
  return $
    MT.Package
      libName
      (exModules ++ moduleNames)
      hsSrcDirs
      (if pkgType == MT.Library
         then []
         else dependencies)
      pkgType
      dumpDir

mkPackageFromExecutable :: String -> String -> String -> MT.Builder -> Cabal.Executable -> IO (MT.Package MT.ModuleMetadata)
mkPackageFromExecutable projectName projectVersion projectPath builder executable = do
  let exeName = Cabal.unUnqualComponentName $ Cabal.exeName executable
      mainModule = SF.takeBaseName $ Cabal.modulePath executable
  dumpDir <- getDumpDir projectName projectVersion projectPath exeName MT.Executable builder
  (hsSrcDirs, moduleNames, dependencies) <- parseBuildInfo projectPath $ Cabal.buildInfo executable
  modules <- pairModuleWithPath projectPath hsSrcDirs [Cabal.fromString mainModule]
  return $ MT.Package exeName (modules ++ moduleNames) hsSrcDirs dependencies MT.Executable dumpDir

moduleNameToString :: Cabal.ModuleName -> String
moduleNameToString moduleName = DL.intercalate "." $ Cabal.components moduleName

pairModuleWithPath :: String -> [String] -> [Cabal.ModuleName] -> IO [MT.ModuleMetadata]
pairModuleWithPath projectPath hsSrcDirs moduleNames = do
  let moduleTuples =
        map
          (\(src, name) -> (name, SF.addExtension (SF.joinPath [projectPath, src, Cabal.toFilePath name]) "hs"))
          [(x, y) | x <- hsSrcDirs, y <- moduleNames]
  return . map (\(mName, path) -> MT.ModuleMetadata (moduleNameToString mName) path) =<<
    filterM (SD.doesFileExist . snd) moduleTuples

mkMetadataFromModule :: String -> Cabal.ModuleName -> MT.ModuleMetadata
mkMetadataFromModule srcDir moduleName =
  MT.ModuleMetadata (moduleNameToString moduleName) (srcDir SF.</> Cabal.toFilePath moduleName)

{-
Cabal: 
 - library ./dist-newstyle/<platform>-<os>/ghc-<version>/<project>-<version>/build
 dist-newstyle/build/x86_64-osx/ghc-8.10.7/mExport-0.0.1/build
 - executable ./dist-newstyle/<platform>-<os>/ghc-<version>/<project>-<version>/x/<executable-name>/build/<executable-name>/<executable-name>-tmp
 dist-newstyle/build/x86_64-osx/ghc-8.10.7/mExport-0.0.1/x/mexport/build/mexport/mexport-tmp
 - test ./dist-newstyle/<platform>-<os>/ghc-<version>/<project>-<version>/t/<test-name>
 dist-newstyle/build/x86_64-osx/ghc-8.10.7/mExport-0.0.1/t/mexport-test/build/mexport-test/mexport-test-tmp
 - benchmark ./dist-newstyle/<platform>-<os>/ghc-<version>/<project>-<version>/b/<benchmark-name>/build/<benchmark-name>/<benchmark-name>-tmp
 dist-newstyle/build/x86_64-osx/ghc-8.10.7/mExport-0.0.1/b/mexport-src/build/mexport-src/mexport-src-tmp

Stack:
  - library ./.stack-work/dist/<platform>-<os>/Cabal-<cabal-version>/build
  .stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build
  - executable ./.stack-work/dist/<platform>-<os>/Cabal-<cabal-version>/build/<executable-name>/<executable-name>-tmp
    .stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/MExport/mexport-tmp
  - test ./.stack-work/dist/<platform>-<os>/Cabal-<cabal-version>/build/<test-name>/<test-name>-tmp
  .stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/mexport-test/mexport-test-tmp
  - benchmark ./.stack-work/dist/<platform>-<os>/Cabal-<cabal-version>/build/<bench-name>/<bench-name>-tmp
  .stack-work/dist/x86_64-osx/Cabal-3.2.1.0/build/mexport-bench/mexport-bench-tmp
-}
getDumpDir :: String -> String -> String -> String -> MT.PackageType -> MT.Builder -> IO (Maybe String)
getDumpDir _ _ projectDir pkgName pkgType MT.STACK = do
  putStrLn =<< runCommand "stack path --dist-dir"
  mDistDir <-
    return . MU.headMaybe =<<
    filterM (\p -> SD.doesPathExist $ SF.joinPath [projectDir, p]) . lines =<<
    runCommand ("cd " ++ projectDir ++ " && stack path --dist-dir")
  case mDistDir of
    Just distDir ->
      case pkgType of
        MT.Library -> return $ Just $ SF.joinPath [projectDir, distDir, "build"]
        _ -> return $ Just $ SF.joinPath [projectDir, distDir, "build", pkgName, pkgName ++ "-tmp"]
    Nothing -> return Nothing
getDumpDir projectName projectVersion projectDir pkgName pkgType MT.CABAL = do
  let arch = PP.render $ Cabal.pretty Cabal.buildArch
      os = PP.render $ Cabal.pretty Cabal.buildOS
  mVersion <-
    return . MU.headMaybe . filter (\l -> DM.isJust $ (Cabal.simpleParsec l :: DM.Maybe Cabal.Version)) . lines =<<
    runCommand ("cd " ++ projectDir ++ " && cabal exec ghc -- --numeric-version")
  case mVersion of
    Just ghcVersion -> do
      let distDir =
            SF.joinPath
              [ projectDir
              , "dist-newstyle"
              , "build"
              , arch ++ "-" ++ os
              , "ghc-" ++ ghcVersion
              , projectName ++ "-" ++ projectVersion
              ]
      case pkgType of
        MT.Library -> return $ Just $ SF.joinPath [distDir, "build"]
        MT.Executable -> return $ Just $ SF.joinPath [distDir, "x", pkgName, "build", pkgName, pkgName ++ "-tmp"]
        MT.TestSuite -> return $ Just $ SF.joinPath [distDir, "t", pkgName, "build", pkgName, pkgName ++ "-tmp"]
        MT.Benchmark -> return $ Just $ SF.joinPath [distDir, "b", pkgName, "build", pkgName, pkgName ++ "-tmp"]
        MT.SubLibrary -> return $ Just $ distDir --FIXME: Check sub libraries build path
    Nothing -> return Nothing

runCommand :: String -> IO String
runCommand cmd = do
  (errCode, stdOut, stdErr) <- SP.readCreateProcessWithExitCode (SP.shell cmd) ""
  if errCode == SE.ExitSuccess
    then return stdOut
    else error $ "Exception occurred: " ++ stdErr
