module Main where

import Control.Monad

import qualified MExport as ME
import qualified MExport.Config as CC

import qualified Types as T
import qualified Utils as U

main :: IO ()
main = do
  action <- U.getAction
  case action of
    T.ShowVersion -> putStrLn $ "mExport v" <> U.version
    T.Run args -> do
      config <- U.mkConfig CC.defaultConfig args
      void $ ME.mExport config
