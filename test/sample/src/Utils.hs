module Utils where

import Config (Config, _data, getConfig)
import Prelude

printString :: Config -> IO ()
printString config = putStrLn $ _data config
