module Main where

import Control.Monad
import Prelude

import qualified MExport.Config as CC
import qualified MExport as ME

import qualified Types as T (Action(..))
import qualified Utils as U

main :: IO ()
main = do
  action <- U.getAction CC.getConfig
  case action of
    T.ShowVersion -> putStrLn "mExport version 0.0.1"
    T.Run config -> void $ ME.mExport config
