module MExport.Pretty where

import Prelude

import qualified Data.Text as DT
import qualified Language.Haskell.Exts as H

import qualified MExport.Config as CC
import qualified MExport.Types as MT

prettifyModuleExports :: CC.Config -> (MT.Project MT.Module) -> [MT.PrettyModule]
prettifyModuleExports config project =
  let modules = MT.modules project
      style = CC.codeStyle config
      singleListExport = CC.singleListExport config
      indent = CC.indent style
   in (prettyPrintExports (DT.replicate indent " ") singleListExport) <$> modules
  where
    prettyPrintExports :: DT.Text -> Bool -> MT.Module -> MT.PrettyModule
    prettyPrintExports indent singleListExport (MT.Module name path exportSpecList) =
      let (H.ExportSpecList _ exportSpecs) = exportSpecList
       in if null exportSpecs
            then MT.PrettyModule name path ""
            else let code = DT.pack $ H.prettyPrint exportSpecList
                  in if singleListExport
                       then MT.PrettyModule name path code
                       else let finalCode = formatExports indent code
                             in MT.PrettyModule name path finalCode
    formatExports :: DT.Text -> DT.Text -> DT.Text
    formatExports indent code =
      let finalCode =
            "\n" <>
            indent <>
            "( " <>
            (DT.intercalate ("\n" <> indent <> ", ") $
             (DT.replace "\n" "" . DT.stripStart) <$> (DT.splitOn "," $ DT.drop 1 (DT.take (DT.length code - 1) code))) <>
            "\n" <> indent <> ")"
       in finalCode
