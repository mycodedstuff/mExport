module Main where

import qualified Config.Config as CC (Config(..), getConfig)
import qualified Lib.MExport as Lib (mExport)
import qualified Types as T (Context(..))

main :: IO ()
main = do
  let config = CC.getConfig {CC.filterModules = ["*"]}
      context = T.Context {moduleSrc = "./test/sample/src/Main.hs"}
  Lib.mExport config context
