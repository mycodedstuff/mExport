module Main where

import Prelude

import qualified Config.Config as CC
import qualified Lib.MExport as Lib
import Types (Action(..))
import qualified Utils.Utils as U

main :: IO ()
main = do
  action <- U.getAction CC.getConfig
  case action of
    ShowVersion -> putStrLn "mExport version 0.0.1"
    Run config context -> Lib.mExport config context
