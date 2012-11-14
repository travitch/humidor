module Main ( main ) where

import Control.Monad ( forM_ )
import Data.List ( intercalate )
import Text.Printf

import Smoke.C
import Smoke.Gen

main :: IO ()
main = do
  smokeModules <- smokeInitialize
  let conf = GeneratorConfig (const "Qt.Gui") "/tmp/genqt"
  mapM_ (generateSmokeModule conf) smokeModules
  {-
  putStrLn ("Found " ++ show (length smokeModules) ++ " smoke modules")
  mapM_ printSmoke smokeModules
  return ()

printSmoke :: SmokeModule -> IO ()
printSmoke m = do
  _ <- printf "Module: %s\n" (smokeModuleName m)
  forM_ (smokeModuleClasses m) $ \c -> do
    _ <- printf "  %s [%s]\n" (smokeClassName c) (parentClassString c)
    forM_ (smokeClassMethods c) $ \f -> do
      printf "    %s %s(%s)\n" (returnTypeString f) (smokeMethodName f) (argumentTypeString f)

parentClassString :: SmokeClass -> String
parentClassString = intercalate ", " . smokeClassParents

returnTypeString :: SmokeMethod -> String
returnTypeString = csmokeTypeName . smokeMethodRet

argumentTypeString :: SmokeMethod -> String
argumentTypeString m =
  intercalate ", " (map csmokeTypeName (smokeMethodArgs m))
-}