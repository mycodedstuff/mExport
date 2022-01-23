module Main where

import Prelude ( ($), IO, putStrLn )
import qualified Config as C ( Config(_addExclamation), getConfig )
import Control.Monad ( when )
import Utils ( (>:>), printString )

main :: IO ()
main = do
  when (C._addExclamation config) $ putStrLn "Using exclamation"
  printString config $ "Hello" >:> "World"

config :: C.Config
config = C.getConfig
