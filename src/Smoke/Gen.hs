{-# LANGUAGE OverloadedStrings #-}
-- | Generates one Haskell module per class in the Smoke module.
module Smoke.Gen (
  GeneratorConfig(..),
  defaultGeneratorConfig,
  generateSmokeModule
  ) where

import Control.Monad.Reader
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
import Smoke.Gen.Cabal
import Smoke.Gen.Classes
import Smoke.Gen.Monad
import Smoke.Gen.Enum
import Smoke.Gen.PrivateModule
import Smoke.Gen.Util

-- FIXME: Do not generate a module for external classes (rather, import
-- them into the types module and re-export them)

-- | Generate Haskell modules for each class in a SmokeModule.  The
-- files are placed in the given @destdir@ (from the config).
generateSmokeModule :: GeneratorConfig -> SmokeModule -> IO ()
generateSmokeModule conf m =
  runReaderT action (conf, smokeModuleName m)
  where
    ch = classHierarchy m
    action = do
      -- Now create a module for each Qt class
      mapM_ (generateSmokeClass m ch) (smokeModuleClasses m)
      -- Take the template cabal file and add the list of generated
      -- modules.
      expandCabalTemplate m
      -- Create a module declaring all of the classes for object types
      generateSmokeClassesModule m
      -- Create a private module with some helpers for this SmokeModule.
      -- This includes the method invoke dispatcher.
      generateSmokeModulePrivate

-- | FIXME: Enums (which can be found by checking method flags) should
-- be represented as top-level ADTs.  The mapping to numeric IDs can
-- be done via Enum instances.  The definitions of the constant values
-- should be private and defined via top-level unsafePerformIO calls
-- (CAFs)
generateSmokeClass :: SmokeModule -> ClassHierarchy -> SmokeClass -> Gen ()
generateSmokeClass smod h c
  | skipClass c = return ()
  | otherwise = do
    mname <- classModuleName c
    fname <- moduleToFilePath mname
    let loc = SrcLoc fname 0 0
    lift $ createDirectoryIfMissing True (dropFileName fname)
    (eExp, edecl) <- makeEnumsForClass smod c
    -- Make a data type declaration for the class, and declare it as an
    -- instance of this class as well as all of its parent classes
    (tdsExp, tds) <- makeClassTypeDefinition loc h c
    -- Make a typeclass for each non-constructor method
    (tcExp, tcMap) <- foldM (makeClassForMethod loc c) mempty (smokeClassMethods c)
    mimp <- privateModuleImport
    cimp <- classesModuleImport
    let tcs = M.elems tcMap
        fimp = ImportDecl loc (ModuleName "Foreign.Ptr") False False Nothing Nothing Nothing
        simp = ImportDecl loc (ModuleName "Smoke") False False Nothing Nothing Nothing
        prag = LanguagePragma loc [Ident "MultiParamTypeClasses"]
        decls = edecl ++ tds ++ tcs
        -- Make sure to put the class exports last
        exports = tdsExp : eExp ++ tcExp
        modNam = ModuleName $ T.unpack mname
        m = Module loc modNam [prag] Nothing (Just exports) [fimp,simp,mimp,cimp] decls
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
      cname = Ident $ T.unpack $ dropOuterClass $ smokeClassName c
      conDecl = QualConDecl loc [] [] $ RecDecl cname [([unwrapFunctionName], UnBangedTy conDataType)]
      ddecl = DataDecl loc NewType [] cname [] [conDecl] []
      thisType = TyCon (UnQual cname)

  -- Now make it an instance of its own class, as well as all of its
  -- parents.
  let is = smokeClassName c : smokeClassTransitiveParents h c
      is' = map dropOuterClass is
  instances <- mapM (makeSuperclassInstances thisType) is'
  return (EAbs (UnQual cname), ddecl : instances)
  where
    makeSuperclassInstances t superclass = do
      cmangler <- askModuleConf generatorClassNameMangler
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
  | methodIsOperator m || methodIsDestructor m || methodIsCopyConstructor m || methodIsEnum m = return a
  | otherwise = do
    nameMap <- askModuleConf generatorMethodClassNameMap
    cmangler <- askModuleConf generatorConstructorMangler
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
    methodName = sanitizeMethodName $ smokeMethodName m

sanitizeMethodName :: Text -> Text
sanitizeMethodName t
  | t == "data" = "data_"
  | t == "type" = "type_"
  | t == "instance" = "instance_"
  | otherwise = t

dropOuterClass :: Text -> Text
dropOuterClass s
  | T.isInfixOf "::" s = T.dropWhile (==':') $ T.dropWhile (/=':') s
  | otherwise = s

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
-- >       qtguiInvokeMethod classIx methIx (unwrapQPrinter self) a
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
      cmangler <- askModuleConf generatorClassNameMangler
      let cname = cmangler $ dropOuterClass $ smokeClassName c
          constraint = UnQual $ Ident $ T.unpack cname
      return $ TyForall Nothing [ClassA constraint [selfVar]] ft
    True -> return argsType
  where
    argTy = TyVar $ Ident "xargs"
    retTy = TyVar $ Ident "xret"
    ioTy = TyCon $ UnQual $ Ident "IO"
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