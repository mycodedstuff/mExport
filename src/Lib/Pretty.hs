module Lib.Pretty where

import Prelude

import qualified Data.Text as DT
import qualified Language.Haskell.Exts as H

import qualified Lib.Config as CC

prettyPrint :: CC.Config -> H.ExportSpecList H.SrcSpanInfo -> DT.Text
prettyPrint config ast =
  let code = DT.pack $ H.prettyPrint ast
      style = CC.codeStyle config
      singleListExport = CC.singleListExport config
   in if singleListExport
        then code
        else let indent = DT.replicate (CC.indent style) " "
                 finalCode =
                   "\n" <>
                   indent <>
                   "( " <>
                   (DT.intercalate ("\n" <> indent <> ",") $ DT.splitOn "," $ DT.drop 1 (DT.take (DT.length code - 1) code)) <>
                   "\n" <> indent <> ")"
              in finalCode
