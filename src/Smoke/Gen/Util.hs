{-# LANGUAGE OverloadedStrings #-}
module Smoke.Gen.Util (
  locationForClass,
  classModuleName,
  skipClass
  ) where

import qualified Data.Text as T
import System.FilePath

import Smoke.C
import Smoke.Gen.Monad

locationForClass :: SmokeClass -> Gen FilePath
locationForClass c = do
  moduleName <- askModuleName
  destDir <- askModuleConf generatorDestDir
  modNameMap <- askModuleConf generatorModuleNameMap
  let modPath = moduleToPath (modNameMap moduleName)
  return $ destDir </> "src" </> modPath </> typeModuleName <.> "hs"
  where
    cname = smokeClassName c
    typeModuleName = T.unpack $ T.replace "::" "/" cname

skipClass :: SmokeClass -> Bool
skipClass c = classIsUndefined c || isIterator
  where
    isIterator = T.isInfixOf "::iterator" (smokeClassName c)

classModuleName :: SmokeClass -> Gen String
classModuleName c = do
  moduleName <- askModuleName
  modNameMap <- askModuleConf generatorModuleNameMap
  return $ T.unpack (modNameMap moduleName) <.> T.unpack typeModuleName
  where
    cname = smokeClassName c
    typeModuleName = T.replace "::" "." cname
