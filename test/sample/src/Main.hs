module Main where

import qualified Config as C (Config(..), getConfig)
import Prelude
import Utils (printString)

main :: IO ()
main = printString config

config :: C.Config
config = C.getConfig
