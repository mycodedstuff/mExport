module MExport.Utils.Utils
  ( moduleToPath
  , findModules
  , headMaybe
  , findFirstInText
  , findCabalFile
  ) where

import Control.Monad

import qualified Data.List as DL
import qualified Data.Text as DT
import qualified Data.Text.Internal.Search as DTS
import qualified System.Directory as SD
import qualified System.FilePath as SF

import qualified MExport.Config as MC

moduleToPath :: String -> String
moduleToPath =
  map
    (\c ->
       if c == '.'
         then '/'
         else c)

headMaybe :: [a] -> Maybe a
headMaybe arr =
  if null arr
    then Nothing
    else Just $ head arr

findFirstInText :: DT.Text -> DT.Text -> Maybe Int
findFirstInText needle haystack = headMaybe $ DTS.indices needle haystack

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

-- Find the project's .cabal file
findCabalFile :: String -> IO String
findCabalFile dir = do
  maybeFile <- return . headMaybe . filter (SF.isExtensionOf "cabal") =<< SD.listDirectory dir
  case maybeFile of
    Just file -> return $ dir SF.</> file
    Nothing -> do
      let upDir = SF.takeDirectory dir
      if upDir == dir
        then error "No .cabal file found!"
        else findCabalFile upDir
