module MExport.Utils.Writer where

import qualified Data.Foldable as DF
import qualified Data.List as DL
import qualified Data.Maybe as DM
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.IO as TIO

import qualified MExport.Types as MT
import qualified MExport.Utils.Utils as MU

writeExports :: [MT.PrettyModule] -> IO ()
writeExports modules = do
  DF.forM_
    modules
    (\(MT.PrettyModule moduleName modulePath moduleExport maybeCoords) -> do
       moduleContent <- TIO.readFile modulePath
       moduleText <-
         case maybeCoords of
           Nothing -> do
             let mNameIndex = (length moduleName) + (DM.fromMaybe 7 $ MU.findFirstInText (DT.pack moduleName) moduleContent)
                 mWhereIndex = (DM.fromMaybe ((length moduleName) + 8) $ MU.findFirstInText "where" moduleContent)
                 moduleStart = DT.take mNameIndex moduleContent
                 moduleRest = DT.drop (mWhereIndex + 5) moduleContent
                 moduleExports =
                   if DT.null moduleExport
                     then moduleExport
                     else "\n" <> moduleExport
             return $ moduleStart <> moduleExports <> " where" <> moduleRest
           Just (MT.XCoord l1 c1 l2 _) -> do
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
