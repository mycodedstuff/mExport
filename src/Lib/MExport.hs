module Lib.MExport
  ( mExport
  ) where

import Prelude

import Control.Monad
import qualified Data.List as DL
import qualified Language.Haskell.Exts as H
import Language.Haskell.Exts.SrcLoc
import qualified Language.Haskell.Interpreter as I
import Language.Haskell.Interpreter (GhcError(..), Interpreter, InterpreterError(..))

import qualified Config.Config as CC
import qualified Lib.Parser as LP
import Types as T

mExport :: CC.Config -> Context -> IO ()
mExport config context = do
  r <- I.runInterpreter $ I.liftIO $ LP.parser (T.moduleSrc context)
  case r of
    Left err -> putStrLn $ errorString err
    Right () -> return ()

errorString :: I.InterpreterError -> String
errorString (WontCompile es) = DL.intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (I.GhcError e) = e
errorString e = show e
