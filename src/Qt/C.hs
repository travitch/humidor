{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Qt.C where

import Control.Monad ( foldM )
import Data.Map ( Map )
import qualified Data.Map as M
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

data Smoke
type SmokeHandle = Ptr Smoke

newtype Index = I Int

-- FIXME: Figure out sizes and alignments in a configure script or
-- something

data CSmokeClass =
  CSmokeClass { csmokeClassName :: String
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
    return CSmokeClass { csmokeClassName = cname
                       , csmokeClassExternal = extByte /= 0
                       , csmokeClassParents = toIndex parents
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
    return $ CSmokeMethod { csmokeMethodClassIndex = toIndex cid
                          , csmokeMethodNameIndex = toIndex name
                          , csmokeMethodArgsIndex = toIndex args
                          , csmokeMethodNumArgs = fromIntegral numArgs
                          , csmokeMethodFlags = flags
                          , csmokeMethodRetIndex = toIndex ret
                          , csmokeMethodMethodIndex = toIndex meth
                          }
  poke = undefined

data CSmokeType =
  CSmokeType { csmokeTypeName :: String
             , csmokeTypeClassIndex :: Index
             , csmokeTypeFlags :: CUInt
             }
type SmokeTypeHandle = Ptr CSmokeType

instance Storable CSmokeType where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = do
    sp <- c_smokeTypeName ptr
    name <- peekCString sp
    cid <- c_smokeTypeClassId ptr
    flags <- c_smokeTypeFlags ptr
    return $ CSmokeType name (toIndex cid) flags
  poke = undefined

toIndex :: CInt -> Index
toIndex = I . fromIntegral

fromShort :: CShort -> Index
fromShort = I . fromIntegral

ptrSize :: Int
ptrSize = sizeOf (nullPtr :: Ptr ())

indexSize :: Int
indexSize = sizeOf (undefined :: CShort)

foreign import ccall "smokeInitialize" c_smokeInitialize :: IO (Ptr SmokeHandle)
foreign import ccall "smokeClasses" c_smokeClasses :: SmokeHandle -> IO SmokeClassHandle
foreign import ccall "smokeNumClasses" c_smokeNumClasses :: SmokeHandle -> IO CInt
foreign import ccall "smokeMethods" c_smokeMethods :: SmokeHandle -> IO SmokeMethodHandle
foreign import ccall "smokeNumMethods" c_smokeNumMethods :: SmokeHandle -> IO CInt
foreign import ccall "smokeMethodNames" c_smokeMethodNames :: SmokeHandle -> IO (Ptr CString)
foreign import ccall "smokeNumMethodNames" c_smokeNumMethodNames :: SmokeHandle -> IO CInt
foreign import ccall "smokeTypes" c_smokeTypes :: SmokeHandle -> IO SmokeTypeHandle
foreign import ccall "smokeNumTypes" c_smokeNumTypes :: SmokeHandle -> IO CInt
foreign import ccall "smokeArgumentList" c_smokeArgumentList :: SmokeHandle -> IO (Ptr CShort)
foreign import ccall "smokeModuleName" c_smokeModuleName :: SmokeHandle -> IO CString
foreign import ccall "free" c_free :: Ptr () -> IO ()
foreign import ccall "smokeClassName" c_smokeClassName :: SmokeClassHandle -> IO CString
foreign import ccall "smokeClassExternal" c_smokeClassExternal :: SmokeClassHandle -> IO CInt
foreign import ccall "smokeClassParents" c_smokeClassParents :: SmokeClassHandle -> IO CInt
foreign import ccall "smokeClassFlags" c_smokeClassFlags :: SmokeClassHandle -> IO CUInt
foreign import ccall "smokeClassSize" c_smokeClassSize :: SmokeClassHandle -> IO CInt
foreign import ccall "smokeMethodClassId" c_smokeMethodClassId :: SmokeMethodHandle -> IO CInt
foreign import ccall "smokeMethodName" c_smokeMethodName :: SmokeMethodHandle -> IO CInt
foreign import ccall "smokeMethodArgs" c_smokeMethodArgs :: SmokeMethodHandle -> IO CInt
foreign import ccall "smokeMethodNumArgs" c_smokeMethodNumArgs :: SmokeMethodHandle -> IO CInt
foreign import ccall "smokeMethodFlags" c_smokeMethodFlags :: SmokeMethodHandle -> IO CUInt
foreign import ccall "smokeMethodRet" c_smokeMethodRet :: SmokeMethodHandle -> IO CInt
foreign import ccall "smokeMethodMethod" c_smokeMethodMethod :: SmokeMethodHandle -> IO CInt
foreign import ccall "smokeTypeName" c_smokeTypeName :: SmokeTypeHandle -> IO CString
foreign import ccall "smokeTypeClassId" c_smokeTypeClassId :: SmokeTypeHandle -> IO CInt
foreign import ccall "smokeTypeFlags" c_smokeTypeFlags :: SmokeTypeHandle -> IO CUInt

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

loadSmokeModule :: SmokeHandle -> IO SmokeModule
loadSmokeModule h = do
  mnp <- c_smokeModuleName h
  modName <- peekCString mnp
  classPtrs <- smokeClasses h
  -- The first one is blank
  methodPtrs <- smokeMethods h
  methodNames <- c_smokeMethodNames h
  let classIdMap = M.fromList $ zip [1..] classPtrs
      s0 = fmap fromCClass classIdMap
  cs <- foldM (buildSmokeClasses methodNames) s0 methodPtrs
  return SmokeModule { smokeModuleName = modName
                     , smokeModuleClasses = M.elems cs
                     }

fromCClass :: CSmokeClass -> SmokeClass
fromCClass cc = SmokeClass { smokeClassMethods = []
                           , smokeClassExternal = csmokeClassExternal cc
                           , smokeClassParents = []
                           , smokeClassFlags = csmokeClassFlags cc
                           , smokeClassName = csmokeClassName cc
                           }

buildSmokeClasses :: Ptr CString
                     -> Map Int SmokeClass
                     -> CSmokeMethod
                     -> IO (Map Int SmokeClass)
buildSmokeClasses methodNames acc cmeth = do
  -- Look up the class id of the current method and add this method to
  -- it.
  let I nameIndex = csmokeMethodNameIndex cmeth
      I cindex = csmokeMethodClassIndex cmeth
  mname <- peekElemOff methodNames nameIndex >>= peekCString
  let smeth = SmokeMethod { smokeMethodName = mname
                          , smokeMethodArgs = []
                          , smokeMethodFlags = csmokeMethodFlags cmeth
                          , smokeMethodRet = 0
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
  SmokeMethod { smokeMethodName :: String
              , smokeMethodArgs :: [Int]
              , smokeMethodFlags :: CUInt
              , smokeMethodRet :: Int
              , smokeMethodIndex :: Index
              }

data SmokeClass =
  SmokeClass { smokeClassName :: String
             , smokeClassMethods :: [SmokeMethod]
             , smokeClassExternal :: Bool
             , smokeClassParents :: [SmokeClass]
             , smokeClassFlags :: CUInt
             }

data SmokeModule =
  SmokeModule { smokeModuleClasses :: [SmokeClass]
              , smokeModuleName :: String
              }

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
