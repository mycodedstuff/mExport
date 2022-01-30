module MExport.Utils
  ( moduleToPath
  , writeExports
  , findModules
  ) where

import Control.Monad
import Prelude

import qualified Data.Foldable as DF
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.IO as TIO
import qualified Data.Text.Internal.Search as DTS
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

headMaybe :: [a] -> Maybe a
headMaybe arr =
  if null arr
    then Nothing
    else Just $ head arr

findFirstInText :: DT.Text -> DT.Text -> Maybe Int
findFirstInText needle haystack = headMaybe $ DTS.indices needle haystack

writeExports :: [T.PrettyModule] -> IO ()
writeExports modules = do
  forM_
    modules
    (\(T.PrettyModule moduleName modulePath moduleExport maybeCoords) -> do
       moduleContent <- TIO.readFile modulePath
       moduleText <-
         case maybeCoords of
           Nothing -> do
             let mNameIndex = (length moduleName) + (DM.fromMaybe 7 $ findFirstInText (DT.pack moduleName) moduleContent)
                 mWhereIndex = (DM.fromMaybe ((length moduleName) + 8) $ findFirstInText "where" moduleContent)
                 moduleStart = DT.take mNameIndex moduleContent
                 moduleRest = DT.drop (mWhereIndex + 5) moduleContent
                 moduleExports =
                   if DT.null moduleExport
                     then moduleExport
                     else "\n" <> moduleExport
             return $ moduleStart <> moduleExports <> " where" <> moduleRest
           Just (T.XCoord l1 c1 l2 _) -> do
             let moduleLines = DT.lines moduleContent
                 moduleStart = DL.take l1 moduleLines
                 moduleRest = DL.drop l2 moduleLines
                 moduleExports =
                   if DT.null moduleExport
                     then []
                     else DT.lines $ moduleExport <> " where"
                 moduleStart' =
                   DF.toList $
                   DS.mapWithIndex
                     (\idx line ->
                        if l1 == idx + 1
                          then let ln = DT.take (c1 - 1) line
                                in if DT.null moduleExport
                                     then ln <> " where"
                                     else ln
                          else line)
                     (DS.fromList moduleStart)
             return $ DT.unlines $ moduleStart' ++ moduleExports ++ moduleRest
       putStrLn $ "Writing exports to " ++ modulePath
       TIO.writeFile modulePath moduleText)
  putStrLn $ "Task completed!"

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
