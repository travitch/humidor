{-# LANGUAGE OverloadedStrings #-}
-- | Generates one Haskell module per class in the Smoke module.
module Smoke.Gen (
  GeneratorConfig(..),
  defaultGeneratorConfig,
  generateSmokeModule
  ) where

import Control.Monad.Reader
import qualified Data.Char as C
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as T
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax
import System.Directory
import System.FilePath

import Smoke.C

data GeneratorConfig =
  GeneratorConfig { generatorModuleNameMap :: Text -> Text
                    -- ^ A function to modify the Smoke module name
                    -- (perhaps stripping off a prefix) used as the root
                    -- of all generated Haskell modules. (default: id)
                  , generatorMethodClassNameMap :: Text -> Text
                    -- ^ A transformation applied to the name of a
                    -- method to produce the typeclass name for that
                    -- method.  This must begin with a capital
                    -- letter. (default: HasMethod+{capitalize})
                  , generatorConstructorMangler :: Text -> Text
                    -- ^ The name mangling to use for constructors.
                    -- (default: new+{constructor})
                  , generatorDestDir :: FilePath
                    -- ^ The directory in which all of the generated
                    -- Haskell module source files will be placed
                  }

-- | A reasonable default configuration
defaultGeneratorConfig :: FilePath -> GeneratorConfig
defaultGeneratorConfig destDir =
  GeneratorConfig { generatorModuleNameMap = id
                  , generatorMethodClassNameMap = mtcname
                  , generatorConstructorMangler = conName
                  , generatorDestDir = destDir
                  }
  where
    mtcname t = "HasMethod" `mappend` capitalize t
    capitalize t =
      case T.unpack t of
        [] -> t
        c:rest -> T.pack $ C.toUpper c : rest
    conName = mappend "new"

type Gen = ReaderT (GeneratorConfig, Text) IO

-- | Generate Haskell modules for each class in a SmokeModule.  The
-- files are placed in the given @destdir@ (from the config).
generateSmokeModule :: GeneratorConfig -> SmokeModule -> IO ()
generateSmokeModule conf m =
  runReaderT action (conf, smokeModuleName m)
  where
    action = mapM_ generateSmokeClass (smokeModuleClasses m)

locationForClass :: SmokeClass -> Gen FilePath
locationForClass c = do
  moduleName <- asks snd
  destDir <- asks (generatorDestDir . fst)
  modNameMap <- asks (generatorModuleNameMap . fst)
  let undot ch = if ch == '.' then '/' else ch
      modPath = T.unpack $ T.map undot (modNameMap moduleName)
  return $ destDir </> modPath </> typeModuleName <.> "hs"
  where
    cname = smokeClassName c
    typeModuleName = T.unpack $ T.replace "::" "/" cname

classModuleName :: SmokeClass -> Gen String
classModuleName c = do
  moduleName <- asks snd
  modNameMap <- asks (generatorModuleNameMap . fst)
  return $ T.unpack (modNameMap moduleName) <.> T.unpack typeModuleName
  where
    cname = smokeClassName c
    typeModuleName = T.replace "::" "." cname

-- | FIXME: Enums (which can be found by checking method flags) should
-- be represented as top-level ADTs.  The mapping to numeric IDs can
-- be done via Enum instances.  The definitions of the constant values
-- should be private and defined via top-level unsafePerformIO calls
-- (CAFs)
generateSmokeClass :: SmokeClass -> Gen ()
generateSmokeClass c = do
  fname <- locationForClass c
  let loc = SrcLoc fname 0 0
  lift $ createDirectoryIfMissing True (dropFileName fname)
  mname <- classModuleName c
  -- Make a typeclass for each non-constructor method
  tcMap <- foldM (makeClassForMethod loc c) mempty (smokeClassMethods c)

  let tcs = M.elems tcMap
      prag = LanguagePragma loc [Ident "MultiParamTypeClasses"]
      m = Module loc (ModuleName mname) [prag] Nothing Nothing [] tcs
      s = prettyPrint m
  lift $ writeFile fname s

-- | Make a typeclass for the method (if a typeclass for another
-- method of the same name hasn't already been made).  Instances will
-- be added in a later pass.
--
-- > class HasMethodAddLine xargs xr where
-- >   addLine :: (IsAQGraphicsScene a) => a -> xargs -> xr
--
-- Note, will need to make special provisions for constructors (they
-- return a fixed type and have no self argument).  Do not forget to
-- ignore destructors.
makeClassForMethod :: SrcLoc -> SmokeClass -> Map Text Decl -> SmokeMethod -> Gen (Map Text Decl)
makeClassForMethod loc c acc m
  | methodIsDestructor m || methodIsCopyConstructor m || methodIsEnum m = return acc
  | otherwise = do
    nameMap <- asks (generatorMethodClassNameMap . fst)
    cmangler <- asks (generatorConstructorMangler . fst)
    case M.lookup methodName acc of
      Just _ -> return acc
      Nothing -> do
        let ctx = []
            cname = Ident $ T.unpack $ nameMap methodName
            mname = case methodIsConstructor m of
              False -> Ident $ T.unpack methodName
              True -> Ident $ T.unpack (cmangler methodName)
            argsVar = UnkindedVar $ Ident "xargs"
            retVar = UnkindedVar $ Ident "xret"
            mtype = makeMethodType c m
            mdecl = ClsDecl $ TypeSig loc [mname] mtype
            tc = ClassDecl loc ctx cname [argsVar, retVar] [] [mdecl]
        return $ M.insert methodName tc acc
  where
    methodName = smokeMethodName m

makeMethodType :: SmokeClass -> SmokeMethod -> Type
makeMethodType c m =
  case methodIsConstructor m of
    False -> TyForall Nothing [ClassA cname [selfVar]] ft
    True -> argsType
  where
    argTy = TyVar $ Ident "xargs"
    retTy = TyVar $ Ident "xret"
    ioTy = TyCon $ UnQual $ Ident "Qt"
    argsType = TyFun argTy (TyApp ioTy retTy)
    ft = TyFun selfVar argsType
    selfVar = TyVar $ Ident "self"
    cname = UnQual $ Ident $ ("IsA" ++ T.unpack (smokeClassName c))

-- Note, using the IsAX constraints here is very useful because we can
-- define all of the IsAX classes in a convenient base package.  Then
-- we can define the X here and add all of the instances we need.
