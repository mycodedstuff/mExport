module Utils (
  module Config,
  printString,
  (>:>)
) where

import Config ( Config(_addExclamation) )
import Prelude ( IO, String, ($), (++), (.), putStrLn )

printString :: Config -> String -> IO ()
printString config str =
  putStrLn $
  if _addExclamation config
    then str ++ "!"
    else str

(>:>) :: String -> String -> String
(>:>) = (++) . (++ " ")

infixl 6 >:>
