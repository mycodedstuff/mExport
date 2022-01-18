module Utils where

import Config (Config, _addExclamation, getConfig)
import Prelude

printString :: Config -> String -> IO ()
printString config str =
  putStrLn $
  if _addExclamation config
    then str ++ "!"
    else str

(>:>) :: String -> String -> String
(>:>) = (++) . (++ " ")

infixl 6 >:>
