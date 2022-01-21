module MExport.Utils
  ( moduleToPath
  , customExtensions
  , writeExports
  , readProject
  ) where

import Control.Monad
import Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Data.Text as DT
import qualified Data.Text.IO as TIO
import qualified Data.Text.Internal.Search as DTS
import qualified Language.Haskell.Exts as H
import qualified System.Directory as SD
import qualified System.FilePath as SF

import qualified MExport.Types as T

moduleToPath :: String -> String
moduleToPath =
  map
    (\c ->
       if c == '.'
         then '/'
         else c)

-- TODO: Add options to provide extensions
customExtensions :: [H.Extension]
customExtensions = map H.EnableExtension [H.TypeApplications]

headMaybe :: [a] -> Maybe a
headMaybe arr =
  if null arr
    then Nothing
    else Just $ head arr

findFirstInText :: DT.Text -> DT.Text -> Maybe Int
findFirstInText needle haystack = headMaybe $ DTS.indices needle haystack

writeExports :: HM.HashMap String DT.Text -> String -> IO ()
writeExports moduleMap mainSrcDir = do
  let modules = HM.keys moduleMap
  forM_
    modules
    (\moduleName -> do
       let modulePath = SF.joinPath [mainSrcDir, ((moduleToPath moduleName) <> ".hs") :: SF.FilePath]
       moduleContent <- TIO.readFile modulePath
       let mNameIndex = (length moduleName) + (DM.fromMaybe 7 $ findFirstInText (DT.pack moduleName) moduleContent)
           mWhereIndex = (DM.fromMaybe ((length moduleName) + 8) $ findFirstInText "where" moduleContent)
           moduleStart = DT.take mNameIndex moduleContent
           moduleRest = DT.drop (mWhereIndex + 5) moduleContent
           moduleExport = DM.fromMaybe "" $ HM.lookup moduleName moduleMap
       TIO.writeFile modulePath (moduleStart <> moduleExport <> " where" <> moduleRest))

readProject :: String -> IO T.Project
readProject projectPath = do
  pathIsDir <- SD.doesDirectoryExist projectPath
  let rootDir =
        if pathIsDir
          then projectPath
          else SF.takeDirectory projectPath
  modules <- findModules rootDir
  return $ T.Project modules rootDir
  where
    findModules :: String -> IO [T.Module]
    findModules filePath = do
      files <- listFiles filePath
      hsFiles <- filterM SD.doesFileExist $ filter ((== ".hs") . SF.takeExtensions) files
      return $ map (T.Module Nothing) hsFiles
    listFiles :: String -> IO [String]
    listFiles dirPath = do
      dirContents <- return . filter (not . DL.isPrefixOf ".") =<< SD.listDirectory dirPath
      return . DL.concat =<<
        mapM
          (\path -> do
             dirExist <- SD.doesDirectoryExist path
             if dirExist
               then listFiles $ SF.joinPath [dirPath, path]
               else return $ [SF.joinPath [dirPath, path]])
          dirContents
