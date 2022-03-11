{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module MExport.Accessor where

import Control.Lens (makeFieldsNoPrefix)

import qualified MExport.Types as MT

makeFieldsNoPrefix ''MT.Project

makeFieldsNoPrefix ''MT.Package

makeFieldsNoPrefix ''MT.ModuleMetadata

makeFieldsNoPrefix ''MT.ModuleInfo
