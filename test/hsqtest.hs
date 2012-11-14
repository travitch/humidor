module Main ( main ) where

import Control.Monad ( forM_ )
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
      printf "    %s\n" (smokeMethodName f)
