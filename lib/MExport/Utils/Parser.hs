module MExport.Utils.Parser where

import qualified Data.Foldable as DF
import qualified Data.List as DL

import qualified GHC.Hs as GHC
import qualified Outputable as GHC
import qualified SrcLoc as GHC

import qualified MExport.Utils.Utils as MU

getExportableCount :: GHC.HsModule GHC.GhcPs -> String -> IO (Maybe Int)
getExportableCount _module typeName = do
  let dataDecls = foldl addIfDataType [] (GHC.unLoc <$> GHC.hsmodDecls _module)
      maybeDataType = MU.headMaybe $ filter (isTypeOfName typeName) dataDecls
      maybeConsDecls = (GHC.dd_cons . GHC.tcdDataDefn) <$> maybeDataType
  case maybeConsDecls of
    Just lConsDecls -> do
      let consDecls = GHC.unLoc <$> lConsDecls
      Just <$>
        DF.foldlM
          (\total decl -> do
             count <- _getCount decl
             return $ total + count)
          (1 + DL.length consDecls) -- Added 1 to count the data type itself with the count of data constructors
          consDecls
    Nothing -> return Nothing
  where
    _getCount :: GHC.ConDecl GHC.GhcPs -> IO Int
    _getCount consDecl = do
      let declDetails = GHC.getConArgs consDecl
      case declDetails of
        GHC.PrefixCon args -> do
          DF.foldlM
            (\count hsType ->
               case hsType of
                 GHC.HsRecTy _ flds -> return $ count + DL.length flds
                 _ -> return count)
            0
            (GHC.unLoc <$> args)
        GHC.RecCon _rec -> return $ DL.length $ GHC.unLoc _rec
        GHC.InfixCon _ _ -> return 0 -- Check when this comes
    addIfDataType arr cur =
      case cur of
        GHC.TyClD _ decl ->
          if GHC.isDataDecl decl
            then decl : arr
            else arr
        _ -> arr
    isTypeOfName :: GHC.Outputable (GHC.IdP pass) => String -> GHC.TyClDecl pass -> Bool
    isTypeOfName name typeDecl = GHC.isDataDecl typeDecl && GHC.showSDocUnsafe (GHC.ppr $ GHC.tcdLName typeDecl) == name
