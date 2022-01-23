module MExport.Pretty where

import Prelude

import qualified Data.List as DL
import qualified Data.Text as DT
import qualified Language.Haskell.Exts as H

import qualified MExport.Config as CC
import qualified MExport.Types as MT

prettifyModuleExports :: CC.Config -> (MT.Project MT.Module) -> [MT.PrettyModule]
prettifyModuleExports config project =
  let modules = MT._modules project
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
  let (MT.PrettyState finalCode _ _) =
        DT.foldl
          (\(MT.PrettyState fCode brac spaces) cur ->
             let (str, _brac, _spaces) =
                   case (brac, spaces, cur) of
                     (_, 0, ' ') -> (" ", brac, spaces + 1)
                     (_, i, ' ') -> ("", brac, spaces)
                     (0, _, '(') -> ("\n" <> indent <> "( ", brac + 1, 0)
                     (_, _, '(') -> ("(", brac + 1, 0)
                     (1, _, ',') -> ("\n" <> indent <> ",", brac, 0)
                     (2, _, ',') -> (",", brac, 0)
                     (i, _, ',') -> ((DT.replicate (i + 1) indent) <> ",", brac, 0)
                     (1, _, ')') -> ("\n" <> indent <> ")", brac - 1, 0)
                     (_, _, ')') -> (")", brac - 1, 0)
                     (_, _, '\n') -> ("", brac, 0)
                     (_, _, x) -> (DT.pack [x], brac, 0)
              in MT.PrettyState (fCode <> str) _brac _spaces)
          (MT.PrettyState "" 0 0)
          code
   in finalCode
