{-# LANGUAGE OverloadedStrings #-}
module Smoke.Gen.Classes (
  generateSmokeClassesModule,
  classesModuleImport
  ) where

import Control.Monad.Reader
import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as T
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

import Smoke.C
import Smoke.Gen.Monad
import Smoke.Gen.Util

generateSmokeClassesModule :: SmokeModule -> Gen ()
generateSmokeClassesModule m = do
  modName <- classesModuleName
  file <- moduleToFilePath modName
  let cs = filter (not . skipClass) (smokeModuleClasses m)
  decls <- mapM toClassDecl cs
  let mname = ModuleName $ T.unpack modName
      ptrImport = ImportDecl dummyLoc (ModuleName "Foreign.Ptr") False False Nothing Nothing Nothing
      hmod = Module dummyLoc mname [] Nothing Nothing [ptrImport] decls
  lift $ writeFile file (prettyPrint hmod)

-- FIXME: If inner classes are a problem here, do not generate them in
-- this module.  Just generate them in the module with their
-- definition since external modules will not be relying on them
toClassDecl :: SmokeClass -> Gen Decl
toClassDecl c = do
  mangler <- askModuleConf generatorClassNameMangler
  let baseName = dropOuterClass $ smokeClassName c
      cname = Ident $ T.unpack $ mangler baseName
      unpacker = "unpack" `mappend` baseName
      tvname = Ident "a"
      utype = TyFun (TyVar tvname) (TyApp (TyCon (UnQual (Ident "Ptr"))) unit_tycon)
      f = ClsDecl $ TypeSig dummyLoc [Ident (T.unpack unpacker)] utype
  return $ ClassDecl dummyLoc [] cname [UnkindedVar tvname] [] [f]

dropOuterClass :: Text -> Text
dropOuterClass s
  | T.isInfixOf "::" s = T.dropWhile (==':') $ T.dropWhile (/=':') s
  | otherwise = s

dummyLoc :: SrcLoc
dummyLoc = SrcLoc "" 0 0

classesModuleName :: Gen Text
classesModuleName = do
  moduleName <- askModuleName
  modNameMap <- askModuleConf generatorModuleNameMap
  return $ modNameMap moduleName `mappend` "Classes"

classesModuleImport :: Gen ImportDecl
classesModuleImport = do
  hmname <- classesModuleName
  return $ ImportDecl dummyLoc (ModuleName (T.unpack hmname)) False False Nothing Nothing Nothing
