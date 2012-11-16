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
                  , generatorClassNameMangler :: Text -> Text
                    -- ^ The name mangling scheme to turn a class name
                    -- into the subclass typeclass marker
                    -- (IsASubclassOfX).
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
                  , generatorClassNameMangler = isAClass
                  , generatorDestDir = destDir
                  }
  where
    mtcname t = "HasMethod" `mappend` capitalize t
    capitalize t =
      case T.unpack t of
        [] -> t
        c:rest -> T.pack $ C.toUpper c : rest
    conName = mappend "new"
    isAClass = mappend "IsA"

type Gen = ReaderT (GeneratorConfig, Text) IO

-- | Generate Haskell modules for each class in a SmokeModule.  The
-- files are placed in the given @destdir@ (from the config).
generateSmokeModule :: GeneratorConfig -> SmokeModule -> IO ()
generateSmokeModule conf m =
  runReaderT action (conf, smokeModuleName m)
  where
    ch = classHierarchy m
    action = mapM_ (generateSmokeClass ch) (smokeModuleClasses m)

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
generateSmokeClass :: ClassHierarchy -> SmokeClass -> Gen ()
generateSmokeClass h c = do
  fname <- locationForClass c
  let loc = SrcLoc fname 0 0
  lift $ createDirectoryIfMissing True (dropFileName fname)
  mname <- classModuleName c
  -- Make a data type declaration for the class, and declare it as an
  -- instance of this class as well as all of its parent classes
  (tdsExp, tds) <- makeClassTypeDefinition loc h c
  -- Make a typeclass for each non-constructor method
  (tcExp, tcMap) <- foldM (makeClassForMethod loc c) mempty (smokeClassMethods c)
  let tcs = M.elems tcMap
      prag = LanguagePragma loc [Ident "MultiParamTypeClasses"]
      decls = tds ++ tcs
      exports = tdsExp : tcExp
      m = Module loc (ModuleName mname) [prag] Nothing (Just exports) [] decls
  lift $ writeFile fname (prettyPrint m)

unwrapFunctionName :: Name
unwrapFunctionName = Ident "unwrapPtr"

-- | Something like:
--
-- > newtype QClassName = QClassName { unwrapPtr :: Ptr () }
-- > instance IsAQClass QClassName where
-- >   unwrapQClass = unwrapPtr
-- > instance IsAQParent QClassName where
-- >   unwrapQParent = unwrapPtr
--
-- The concrete type is opaque to clients (since unwrapPtr is not
-- exported).  All of the method instances call the unwrapQXXX that
-- they want and pass the resulting void* to C++.
--
-- Technically, users can also call these unwrap functions since they
-- will be exported.  Users should not count on this being useful or
-- even a stable part of the interface.
makeClassTypeDefinition :: SrcLoc -> ClassHierarchy -> SmokeClass
                           -> Gen (ExportSpec, [Decl])
makeClassTypeDefinition loc h c = do
  -- First create the data type
  let conDataType = TyApp (TyCon (UnQual (Ident "Ptr"))) unit_tycon
      cname = Ident $ T.unpack $ smokeClassName c
      conDecl = QualConDecl loc [] [] $ RecDecl cname [([unwrapFunctionName], UnBangedTy conDataType)]
      ddecl = DataDecl loc NewType [] cname [] [conDecl] []
      thisType = TyCon (UnQual cname)

  -- Now make it an instance of its own class, as well as all of its
  -- parents.
  let is = smokeClassName c : smokeClassTransitiveParents h c
  instances <- mapM (makeSuperclassInstances thisType) is
  return (EAbs (UnQual cname), ddecl : instances)
  where
    makeSuperclassInstances t superclass = do
      cmangler <- asks (generatorClassNameMangler . fst)
      let className = Ident $ T.unpack $ cmangler superclass
          unwrapName = Ident $ T.unpack $ "unpack" `mappend` superclass
          rhs = UnGuardedRhs $ Var (UnQual unwrapFunctionName)
          insDec = InsDecl $ FunBind [Match loc unwrapName [] Nothing rhs (BDecls [])]
      return $ InstDecl loc [] (UnQual className) [t] [insDec]

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
makeClassForMethod :: SrcLoc -> SmokeClass -> ([ExportSpec], Map Text Decl)
                      -> SmokeMethod -> Gen ([ExportSpec], Map Text Decl)
makeClassForMethod loc c a@(exports, acc) m
  | methodIsDestructor m || methodIsCopyConstructor m || methodIsEnum m = return a
  | otherwise = do
    nameMap <- asks (generatorMethodClassNameMap . fst)
    cmangler <- asks (generatorConstructorMangler . fst)
    case M.lookup methodName acc of
      Just _ -> return a
      Nothing -> do
        mtype <- makeMethodType c m
        let ctx = []
            cname = Ident $ T.unpack $ nameMap methodName
            mname = case methodIsConstructor m of
              False -> Ident $ T.unpack methodName
              True -> Ident $ T.unpack (cmangler methodName)
            argsVar = UnkindedVar $ Ident "xargs"
            retVar = UnkindedVar $ Ident "xret"
            mdecl = ClsDecl $ TypeSig loc [mname] mtype
            tc = ClassDecl loc ctx cname [argsVar, retVar] [] [mdecl]
        return $ (EThingAll (UnQual cname) : exports, M.insert methodName tc acc)
  where
    methodName = smokeMethodName m

-- For instances, generate code like the following
--
-- > import Foreign.Marshal.Array ( allocaArray, advancePtr )
-- > import Foreign.Storable ( peek, poke )
-- >
-- > instance HasMethodFoo (Int, Int) (IO Int) where
-- >   foo self (a1, a2) = do
-- >     allocaArray 3 $ \(a :: Ptr StackItem) -> do
-- >       poke (a `advancePtr` 1) a1
-- >       poke (a `advancePtr` 2) a2
-- >       callQtGuiMethod classIx methIx (unwrapQPrinter self) a
-- >       rv <- peek (castPtr a)
-- >       return (unwrapInt rv)

-- | The type for a method declaration is (for normal methods):
--
-- > (IsAQFoo self) => self -> xargs -> Qt xret
--
-- For constructors, it is
--
-- > xargs -> Qt xret
--
-- Different overloads of each method will have different tuple
-- instances for xargs.
makeMethodType :: SmokeClass -> SmokeMethod -> Gen Type
makeMethodType c m =
  case methodIsConstructor m of
    False -> do
      cmangler <- asks (generatorClassNameMangler . fst)
      let cname = cmangler $ smokeClassName c
          constraint = UnQual $ Ident $ T.unpack cname
      return $ TyForall Nothing [ClassA constraint [selfVar]] ft
    True -> return argsType
  where
    argTy = TyVar $ Ident "xargs"
    retTy = TyVar $ Ident "xret"
    ioTy = TyCon $ UnQual $ Ident "Qt"
    argsType = TyFun argTy (TyApp ioTy retTy)
    ft = TyFun selfVar argsType
    selfVar = TyVar $ Ident "self"

-- Note, using the IsAX constraints here is very useful because we can
-- define all of the IsAX classes in a convenient base package.  Then
-- we can define the X here and add all of the instances we need.


-- A note on overriding virtual methods.
--
-- To extend class C:
--
-- 1) Allocate an instance of C using a constructor (somehow closing
-- over extra values to add data to the underlying class... maybe just
-- use Qt properties)
--
-- > // set up call
-- > void *obj = stack[0].s_voidp;
--
-- 2) Use the 0th method to set a custom SmokeBinding (that has been
-- passed its set of overridden methods).  This is the magic that lets
-- the smoke wrappers override virtual methods.
--
-- > MySmokeBinding b(qtgui_Smoke, {5:overrideSize, 6:overridePaint});
-- > Smoke::StackItem stack[2];
-- > stack[1].s_voidp = &b;
-- > klass->classFn(0, obj, stack);
--
-- The SmokeBinding is always called when a virtual method is invoked.
-- If it returns True, it has handled the call.  If it returns False,
-- the original method is called.  The custom SmokeBinding we use will
-- have to take as an argument its overrides, which will be checked at
-- runtime ine callMethod.
--
-- It seems to be the case that *all* instances must be initialized
-- with a binding.  This SmokeBinding also provides a hook to run code
-- when an object is deleted.
--
-- A possible API design for subclassing:
--
-- > myOverrides = makeOverrides $ M.fromList [ ("paint", myPaint) ]
-- >
-- > newCustomButton lbl = subclass qButtonClass myOverrides
--
-- where myPaint has an appropriate signature and will be wrapped in a
-- FuncPtr by subclass.  qButtonClass will have to be a data CAF
-- decalred in Qt.Gui.QButton that provides enough information for
-- subclass to look up the method index of the thing being overridden.
--
-- This will require manual instance declarations OR TemplateHaskell.
-- Problem, TemplateHaskell and C++ don't seem to always play nicely
-- together.
--
-- subclass should return a function that acts as a constructor.  The
-- question of how to attach new data is difficult - perhaps
-- properties could be used.  Closing over some data would be
-- difficult; maybe we can just add extra data on the SmokeBinding,
-- accessible by IORef.  Properties may be possible with setProperty,
-- which can create new "dynamic properties".
--
-- If a virtual method has several overloads, we may need to pass in
-- smoke-style mangled names.