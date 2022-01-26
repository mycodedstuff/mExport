module MExport.Pretty where

import Prelude

import qualified Data.Text as DT
import qualified Language.Haskell.Exts as H
import qualified Outputable as GHC
import qualified SrcLoc as GHC

import qualified MExport.Config as CC
import qualified MExport.Ghc.Parser as GP
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
            then MT.PrettyModule name path "" Nothing
            else let code = DT.pack $ H.prettyPrint exportSpecList
                  in if singleListExport
                       then MT.PrettyModule name path code Nothing
                       else let finalCode = formatExports indent code
                             in MT.PrettyModule name path finalCode Nothing

formatExports :: DT.Text -> DT.Text -> DT.Text
formatExports indent code =
  let (MT.PrettyState finalCode _ _) =
        DT.foldl
          (\(MT.PrettyState fCode brac spaces) cur ->
             let (str, _brac, _spaces) =
                   case (brac, spaces, cur) of
                     (_, 0, ' ') -> (" ", brac, spaces + 1)
                     (_, _, ' ') -> ("", brac, spaces)
                     (0, _, '(') -> (indent <> "( ", brac + 1, 0)
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

prettifyExports :: CC.Config -> MT.Project GP.Module -> [MT.PrettyModule]
prettifyExports config project =
  let modules = MT._modules project
      style = CC.codeStyle config
      singleListExport = CC.singleListExport config
      indent = DT.replicate (CC.indent style) " "
   in printExports indent singleListExport <$> modules
  where
    printExports :: DT.Text -> Bool -> GP.Module -> MT.PrettyModule
    printExports indent singleListExport (GP.Module name path coords exportSpecs) =
      if null $ GHC.unLoc exportSpecs
        then MT.PrettyModule name path "" coords
        else let code = DT.map replaceSqBrackets $ DT.pack $ GHC.showSDocUnsafe $ GHC.ppr exportSpecs
              in if singleListExport
                   then MT.PrettyModule name path code coords
                   else let finalCode = formatExports indent code
                         in MT.PrettyModule name path finalCode coords
    replaceSqBrackets :: Char -> Char
    replaceSqBrackets =
      \case
        '[' -> '('
        ']' -> ')'
        c -> c
