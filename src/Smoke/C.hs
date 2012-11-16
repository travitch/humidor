{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Smoke.C (
  SmokeModule(..),
  SmokeClass(..),
  SmokeMethod(..),
  methodIsDestructor,
  methodIsConstructor,
  methodIsCopyConstructor,
  methodIsEnum,
  CSmokeType(..),
  smokeInitialize,
  ClassHierarchy,
  classHierarchy,
  smokeClassTransitiveParents
  ) where

import Control.Monad ( foldM )
import Data.Bits
import Data.List ( foldl' )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( mapMaybe )
import Data.Monoid
import qualified Data.Set as S
import Data.Text ( Text )
import qualified Data.Text as T
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

data Smoke
type SmokeHandle = Ptr Smoke

-- FIXME: Figure out sizes and alignments in a configure script or
-- something

type Index = CShort

data CSmokeClass =
  CSmokeClass { csmokeClassName :: Text
              , csmokeClassExternal :: Bool
              , csmokeClassParents :: Index
              , csmokeClassCallMethod :: Ptr ()
              , csmokeClassEnum :: Ptr ()
              , csmokeClassFlags :: CUInt
              , csmokeClassSize :: Int
              }
type SmokeClassHandle = Ptr CSmokeClass

instance Storable CSmokeClass where
  sizeOf _ = 40
  alignment _ = 8
  peek ptr = do
    cnamep <- c_smokeClassName ptr
    cname <- peekCString cnamep
    extByte <- c_smokeClassExternal ptr
    parents <- c_smokeClassParents ptr
    flags <- c_smokeClassFlags ptr
    size <- c_smokeClassSize ptr
    return CSmokeClass { csmokeClassName = T.pack cname
                       , csmokeClassExternal = extByte /= 0
                       , csmokeClassParents = parents
                       , csmokeClassCallMethod = nullPtr
                       , csmokeClassEnum = nullPtr
                       , csmokeClassFlags = flags
                       , csmokeClassSize = fromIntegral size
                       }
  poke = undefined

data CSmokeMethod =
  CSmokeMethod { csmokeMethodClassIndex :: Index
               , csmokeMethodNameIndex :: Index
               , csmokeMethodArgsIndex :: Index
               , csmokeMethodNumArgs :: Int
               , csmokeMethodFlags :: CUInt
               , csmokeMethodRetIndex :: Index
               , csmokeMethodMethodIndex :: Index
               }
type SmokeMethodHandle = Ptr CSmokeMethod

instance Storable CSmokeMethod where
  sizeOf _ = 14
  alignment _ = 2
  peek ptr = do
    cid <- c_smokeMethodClassId ptr
    name <- c_smokeMethodName ptr
    args <- c_smokeMethodArgs ptr
    numArgs <- c_smokeMethodNumArgs ptr
    flags <- c_smokeMethodFlags ptr
    ret <- c_smokeMethodRet ptr
    meth <- c_smokeMethodMethod ptr
    return $ CSmokeMethod { csmokeMethodClassIndex = cid
                          , csmokeMethodNameIndex = name
                          , csmokeMethodArgsIndex = args
                          , csmokeMethodNumArgs = fromIntegral numArgs
                          , csmokeMethodFlags = flags
                          , csmokeMethodRetIndex = ret
                          , csmokeMethodMethodIndex = meth
                          }
  poke = undefined

data CSmokeType =
  CSmokeType { csmokeTypeName :: Text
             , csmokeTypeClassIndex :: Index
             , csmokeTypeFlags :: CUInt
             }
type SmokeTypeHandle = Ptr CSmokeType

instance Storable CSmokeType where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = do
    sp <- c_smokeTypeName ptr
    -- The void type has a null pointer for its name (and zeros for
    -- its other info).  Special case it
    name <- if sp /= nullPtr then peekCString sp else return "void"
    cid <- c_smokeTypeClassId ptr
    flags <- c_smokeTypeFlags ptr
    return $ CSmokeType (T.pack name) cid flags
  poke = undefined

ptrSize :: Int
ptrSize = sizeOf (nullPtr :: Ptr ())

smokeHandles :: IO [SmokeHandle]
smokeHandles = do
  smokes <- c_smokeInitialize
  lst <- go smokes []
  c_free (castPtr smokes)
  return lst
  where
    go pptr acc = do
      ptr <- peek pptr
      if ptr == nullPtr
        then return (reverse acc)
        else go (pptr `plusPtr` ptrSize) (ptr : acc)

smokeInitialize :: IO [SmokeModule]
smokeInitialize = do
  handles <- smokeHandles
  mapM loadSmokeModule handles

initializeArgMapper :: SmokeHandle -> IO ArgumentMapper
initializeArgMapper h = do
  argList <- c_smokeArgumentList h
  typeList <- c_smokeTypes h
  return $ ArgumentMapper argList typeList

loadSmokeModule :: SmokeHandle -> IO SmokeModule
loadSmokeModule h = do
  mnp <- c_smokeModuleName h
  modName <- peekCString mnp
  classPtrs <- smokeClasses h
  methodPtrs <- smokeMethods h
  methodNames <- c_smokeMethodNames h
  argMapper <- initializeArgMapper h
  ilist <- c_smokeInheritanceList h

  let classIdMap = M.fromList $ zip [1..] classPtrs
  classes <- mapM (fromCClass ilist classIdMap) classPtrs
  let s0 = M.fromList $ zip [1..] classes
  cs <- foldM (buildSmokeClasses methodNames argMapper) s0 methodPtrs
  return SmokeModule { smokeModuleName = T.pack modName
                     , smokeModuleClasses = M.elems cs
                     }

fromCClass :: Ptr CShort -> Map Int CSmokeClass -> CSmokeClass -> IO SmokeClass
fromCClass il classIdMap cc = do
  let pix = csmokeClassParents cc
  parentClasses <- untilM (==0) (peekElemOff il) [fromIntegral pix..]
  return SmokeClass { smokeClassMethods = []
                    , smokeClassExternal = csmokeClassExternal cc
                    , smokeClassParents = mapMaybe lookupParent parentClasses
                    , smokeClassFlags = csmokeClassFlags cc
                    , smokeClassName = csmokeClassName cc
                    }
  where
    lookupParent parentIndex = do
      p <- M.lookup (fromIntegral parentIndex) classIdMap
      return (csmokeClassName p)

data ArgumentMapper = ArgumentMapper (Ptr CShort) SmokeTypeHandle

argumentTypes :: ArgumentMapper -> Index -> IO [CSmokeType]
argumentTypes (ArgumentMapper a t) ix = do
  -- use @ix@ as an index into the list of argument lists.  Read off
  -- all of the shorts (using peekElemOff) until one is zero.
  typeIndices <- untilM (==0) (peekElemOff a) [fromIntegral ix..]
  mapM (peekElemOff t . fromIntegral) typeIndices

returnType :: ArgumentMapper -> Index -> IO CSmokeType
returnType (ArgumentMapper _ t) ix =
  peekElemOff t (fromIntegral ix)

buildSmokeClasses :: Ptr CString
                     -> ArgumentMapper
                     -> Map Index SmokeClass
                     -> CSmokeMethod
                     -> IO (Map Index SmokeClass)
buildSmokeClasses methodNames argMapper acc cmeth = do
  -- Look up the class id of the current method and add this method to
  -- it.
  let nameIndex = csmokeMethodNameIndex cmeth
      cindex = csmokeMethodClassIndex cmeth
  mname <- peekElemOff methodNames (fromIntegral nameIndex) >>= peekCString
  ats <- argumentTypes argMapper (csmokeMethodArgsIndex cmeth)
  rt <- returnType argMapper (csmokeMethodRetIndex cmeth)
  let smeth = SmokeMethod { smokeMethodName = T.pack mname
                          , smokeMethodArgs = ats
                          , smokeMethodFlags = csmokeMethodFlags cmeth
                          , smokeMethodRet = rt
                          , smokeMethodIndex = csmokeMethodMethodIndex cmeth
                          }
  case M.lookup cindex acc of
    Nothing -> return acc
    Just klass -> do
      let klass' = klass { smokeClassMethods = smeth : smokeClassMethods klass }
      return $ M.insert cindex klass' acc

smokeClasses :: SmokeHandle -> IO [CSmokeClass]
smokeClasses s = peekArray s c_smokeClasses c_smokeNumClasses

smokeMethods :: SmokeHandle -> IO [CSmokeMethod]
smokeMethods s = peekArray s c_smokeMethods c_smokeNumMethods

data SmokeMethod =
  SmokeMethod { smokeMethodName :: Text
              , smokeMethodArgs :: [CSmokeType]
              , smokeMethodFlags :: CUInt
              , smokeMethodRet :: CSmokeType
              , smokeMethodIndex :: Index
              }

methodIsDestructor :: SmokeMethod -> Bool
methodIsDestructor = (/=0) . (.&. 0x40) . smokeMethodFlags

methodIsCopyConstructor :: SmokeMethod -> Bool
methodIsCopyConstructor = (/=0) . (.&. 0x04) . smokeMethodFlags

methodIsConstructor :: SmokeMethod -> Bool
methodIsConstructor = (/=0) . (.&. 0x20) . smokeMethodFlags

-- | Oddly enough, enum values appear as methods.
methodIsEnum :: SmokeMethod -> Bool
methodIsEnum = (/=0) . (.&. 0x10) . smokeMethodFlags

data SmokeClass =
  SmokeClass { smokeClassName :: Text
             , smokeClassMethods :: [SmokeMethod]
             , smokeClassExternal :: Bool
             , smokeClassParents :: [Text] -- [SmokeClass]
             , smokeClassFlags :: CUInt
             }

data SmokeModule =
  SmokeModule { smokeModuleClasses :: [SmokeClass]
              , smokeModuleName :: Text
              }

-- | A representation of the class hierarchy in a module
data ClassHierarchy = CH (Map Text [Text])

-- | Construct the class hierarchy for a module.
classHierarchy :: SmokeModule -> ClassHierarchy
classHierarchy = CH . foldr buildHierarchy mempty . smokeModuleClasses
  where
    buildHierarchy c =
      M.insert (smokeClassName c) (smokeClassParents c)

-- | Find all transitive parents (excluding the class itself) of the
-- given class.
smokeClassTransitiveParents :: ClassHierarchy -> SmokeClass -> [Text]
smokeClassTransitiveParents (CH h) = S.toList . go mempty . smokeClassName
  where
    go s n =
      case M.lookup n h of
        Nothing -> s
        Just [] -> s
        Just ps -> foldl' go (S.union (S.fromList ps) s) ps

-- Unmarshal helpers

peekArray :: (Integral c, Storable b) =>
             a -> (a -> IO (Ptr b)) -> (a -> IO c) -> IO [b]
peekArray obj arrAccessor sizeAccessor = do
  nElts <- sizeAccessor obj
  arrPtr <- arrAccessor obj
  case nElts == 0 || arrPtr == nullPtr of
    True -> return []
    False -> do
      elts <- foldM (peekCons arrPtr) [] [1..fromIntegral nElts]
      return $ reverse elts
  where
    peekCons ptr acc ix = do
      elt <- peekElemOff ptr ix
      return $ elt : acc

-- Generic helpers
untilM :: (Storable a) => (a -> Bool) -> (Int -> IO a) -> [Int] -> IO [a]
untilM p action ixs = go [] ixs
  where
    go acc [] = return (reverse acc)
    go acc (ix:rest) = do
      elt <- action ix
      case p elt of
        True -> return (reverse acc)
        False -> go (elt : acc) rest

foreign import ccall "smokeInitialize" c_smokeInitialize :: IO (Ptr SmokeHandle)
foreign import ccall "smokeClasses" c_smokeClasses :: SmokeHandle -> IO SmokeClassHandle
foreign import ccall "smokeNumClasses" c_smokeNumClasses :: SmokeHandle -> IO CInt
foreign import ccall "smokeMethods" c_smokeMethods :: SmokeHandle -> IO SmokeMethodHandle
foreign import ccall "smokeNumMethods" c_smokeNumMethods :: SmokeHandle -> IO CInt
foreign import ccall "smokeMethodNames" c_smokeMethodNames :: SmokeHandle -> IO (Ptr CString)
foreign import ccall "smokeNumMethodNames" c_smokeNumMethodNames :: SmokeHandle -> IO CInt
foreign import ccall "smokeTypes" c_smokeTypes :: SmokeHandle -> IO SmokeTypeHandle
foreign import ccall "smokeNumTypes" c_smokeNumTypes :: SmokeHandle -> IO CInt
foreign import ccall "smokeArgumentList" c_smokeArgumentList :: SmokeHandle -> IO (Ptr Index)
foreign import ccall "smokeInheritanceList" c_smokeInheritanceList :: SmokeHandle -> IO (Ptr Index)
foreign import ccall "smokeModuleName" c_smokeModuleName :: SmokeHandle -> IO CString
foreign import ccall "free" c_free :: Ptr () -> IO ()
foreign import ccall "smokeClassName" c_smokeClassName :: SmokeClassHandle -> IO CString
foreign import ccall "smokeClassExternal" c_smokeClassExternal :: SmokeClassHandle -> IO CInt
foreign import ccall "smokeClassParents" c_smokeClassParents :: SmokeClassHandle -> IO Index
foreign import ccall "smokeClassFlags" c_smokeClassFlags :: SmokeClassHandle -> IO CUInt
foreign import ccall "smokeClassSize" c_smokeClassSize :: SmokeClassHandle -> IO CInt
foreign import ccall "smokeMethodClassId" c_smokeMethodClassId :: SmokeMethodHandle -> IO Index
foreign import ccall "smokeMethodName" c_smokeMethodName :: SmokeMethodHandle -> IO Index
foreign import ccall "smokeMethodArgs" c_smokeMethodArgs :: SmokeMethodHandle -> IO Index
foreign import ccall "smokeMethodNumArgs" c_smokeMethodNumArgs :: SmokeMethodHandle -> IO CInt
foreign import ccall "smokeMethodFlags" c_smokeMethodFlags :: SmokeMethodHandle -> IO CUInt
foreign import ccall "smokeMethodRet" c_smokeMethodRet :: SmokeMethodHandle -> IO Index
foreign import ccall "smokeMethodMethod" c_smokeMethodMethod :: SmokeMethodHandle -> IO Index
foreign import ccall "smokeTypeName" c_smokeTypeName :: SmokeTypeHandle -> IO CString
foreign import ccall "smokeTypeClassId" c_smokeTypeClassId :: SmokeTypeHandle -> IO Index
foreign import ccall "smokeTypeFlags" c_smokeTypeFlags :: SmokeTypeHandle -> IO CUInt
foreign import ccall "smokeInvokeMethod" c_smokeInvokeMethod :: SmokeHandle -> Index -> Index -> Ptr () -> Ptr () -> IO ()
