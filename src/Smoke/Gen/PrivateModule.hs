{-# LANGUAGE OverloadedStrings #-}
module Smoke.Gen.PrivateModule (
  generateSmokeModulePrivate,
  privateModuleImport
  ) where

import Control.Monad.Reader
import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as T
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax
import System.FilePath

import Smoke.Gen.Monad
import Smoke.Gen.SmokeImport

privateModuleImport :: Gen ImportDecl
privateModuleImport = do
  hmname <- privateModuleName
  let loc = SrcLoc "" 0 0
  return $ ImportDecl loc (ModuleName (T.unpack hmname)) False False Nothing Nothing Nothing

privateModuleName :: Gen Text
privateModuleName = do
  moduleName <- askModuleName
  modNameMap <- askModuleConf generatorModuleNameMap
  return $ modNameMap moduleName `mappend` "Private"

generateSmokeModulePrivate :: Gen ()
generateSmokeModulePrivate = do
  moduleName <- askModuleName
  hmodName <- privateModuleName
  destDir <- askModuleConf generatorDestDir
  modNameMap <- askModuleConf generatorModuleNameMap
  (invokeExport, invokeDecls) <- generateModuleInvoker
  let hmodFile = destDir </> moduleToPath (modNameMap moduleName) ++ "Private.hs"
      loc = SrcLoc hmodFile 0 0
      exports = [invokeExport]
      imports = [smokeImport]
      m = Module loc (ModuleName (T.unpack hmodName)) [] Nothing (Just exports) imports invokeDecls
  lift $ writeFile hmodFile (prettyPrint m)

generateModuleInvoker :: Gen (ExportSpec, [Decl])
generateModuleInvoker = do
  moduleName <- askModuleName
  let funcName = T.unpack $ moduleName `mappend` "InvokeMethod"
      smokeVarName = T.unpack $ moduleName `mappend` "_Smoke"
      exportClause = EVar (UnQual (Ident funcName))
      varTy = TyCon (UnQual (Ident "SmokeHandle"))
      ffiDecl = ForImp loc CCall (PlaySafe False) smokeVarName (Ident smokeVarName) varTy
      body = App (Var (UnQual (Ident "smokeInvokeMethod"))) (Var (UnQual (Ident smokeVarName)))
      wrapperDecl = FunBind [Match loc (Ident funcName) [] Nothing (UnGuardedRhs body) (BDecls [])]
  return (exportClause, [ffiDecl, wrapperDecl])
  where
    loc = SrcLoc "" 0 0