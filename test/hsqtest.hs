module Main ( main ) where

import Control.Monad ( forM_ )
import Data.List ( intercalate )
import Text.Printf
import Qt.C

main :: IO ()
main = do
  smokeModules <- smokeInitialize
  putStrLn ("Found " ++ show (length smokeModules) ++ " smoke modules")
  mapM_ printSmoke smokeModules
  return ()

printSmoke :: SmokeModule -> IO ()
printSmoke m = do
  _ <- printf "Module: %s\n" (smokeModuleName m)
  forM_ (smokeModuleClasses m) $ \c -> do
    _ <- printf "  %s\n" (smokeClassName c)
    forM_ (smokeClassMethods c) $ \f -> do
      printf "    %s %s(%s)\n" (returnTypeString f) (smokeMethodName f) (argumentTypeString f)

returnTypeString :: SmokeMethod -> String
returnTypeString = csmokeTypeName . smokeMethodRet

argumentTypeString :: SmokeMethod -> String
argumentTypeString m =
  intercalate ", " (map csmokeTypeName (smokeMethodArgs m))