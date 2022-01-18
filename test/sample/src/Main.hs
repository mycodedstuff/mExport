module Main where

import qualified Config as C (Config(..), getConfig)
import Prelude
import Utils ((>:>), printString)

main :: IO ()
main = printString config $ "Hello" >:> "World"

config :: C.Config
config = C.getConfig
