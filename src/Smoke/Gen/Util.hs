{-# LANGUAGE OverloadedStrings #-}
module Smoke.Gen.Util (
  classModuleName,
  skipClass
  ) where

import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as T

import Smoke.C
import Smoke.Gen.Monad

skipClass :: SmokeClass -> Bool
skipClass c = classIsUndefined c || isIterator || isGlobalSpace
  where
    -- This is a fake class that holds all of the globals in Qt.  This
    -- should probably be handled separately.
    isGlobalSpace = "QGlobalSpace" == smokeClassName c
    isIterator = T.isInfixOf "::iterator" (smokeClassName c)

classModuleName :: SmokeClass -> Gen Text
classModuleName c = do
  moduleName <- askModuleName
  modNameMap <- askModuleConf generatorModuleNameMap
  return $ (modNameMap moduleName) `mappend` "." `mappend` typeModuleName
  where
    cname = smokeClassName c
    typeModuleName = T.replace "::" "." cname
