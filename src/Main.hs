module Main where

import Control.Monad
import Prelude

import qualified Lib.Config as CC
import qualified Lib.MExport as Lib

import qualified Types as T (Action(..))
import qualified Utils as U

main :: IO ()
main = do
  action <- U.getAction CC.getConfig
  case action of
    T.ShowVersion -> putStrLn "mExport version 0.0.1"
    T.Run config context -> void $ Lib.mExport config context
