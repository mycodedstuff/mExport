module MExport.Utils
  ( moduleToPath
  , customExtensions
  , writeExports
  , findModules
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

import qualified MExport.Config as MC
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

writeExports :: [T.PrettyModule] -> String -> IO ()
writeExports modules mainSrcDir = do
  forM_
    modules
    (\(T.PrettyModule moduleName modulePath moduleExport) -> do
       moduleContent <- TIO.readFile modulePath
       let mNameIndex = (length moduleName) + (DM.fromMaybe 7 $ findFirstInText (DT.pack moduleName) moduleContent)
           mWhereIndex = (DM.fromMaybe ((length moduleName) + 8) $ findFirstInText "where" moduleContent)
           moduleStart = DT.take mNameIndex moduleContent
           moduleRest = DT.drop (mWhereIndex + 5) moduleContent
       TIO.writeFile modulePath (moduleStart <> moduleExport <> " where" <> moduleRest))

findModules :: MC.Config -> String -> IO [String]
findModules config projectPath = do
  pathIsDir <- SD.doesDirectoryExist projectPath
  let rootDir =
        if pathIsDir
          then projectPath
          else SF.takeDirectory projectPath
  searchHsFiles rootDir $ MC.excludeDir config
  where
    searchHsFiles :: String -> [String] -> IO [String]
    searchHsFiles filePath excludeDir = do
      files <- listFiles filePath excludeDir
      filterM SD.doesFileExist $ filter ((== ".hs") . SF.takeExtensions) files
    listFiles :: String -> [String] -> IO [String]
    listFiles dirPath excludeDir = do
      dirContents <- return . filter (flip DL.notElem excludeDir) =<< SD.listDirectory dirPath
      return . DL.concat =<<
        mapM
          (\path -> do
             let curPath = SF.joinPath [dirPath, path]
             dirExist <- SD.doesDirectoryExist curPath
             if dirExist
               then listFiles curPath excludeDir
               else return [curPath])
          dirContents
