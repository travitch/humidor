{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Control.Applicative
import Options.Applicative

import Smoke.C
import Smoke.Gen

data Opts = Opts { qDestDir :: FilePath
                 , qCabalTemplate :: FilePath
                 , qLibrary :: String
                 }
          deriving (Show)

cmdOpts :: Parser Opts
cmdOpts = Opts
  <$> strOption ( long "output"
                & short 'o'
                & metavar "DIRECTORY"
                & help "The directory to put the generated package in"
                )
  <*> strOption ( long "cabalTemplate"
                & short 'c'
                & metavar "FILE"
                & help "The template cabal file")
  <*> argument str ( metavar "LIBRARY" )

main :: IO ()
main = execParser args >>= realMain
  where
    args = info (helper <*> cmdOpts)
      ( fullDesc
      & progDesc "Generate Haskell bindings for LIBRARY and place the results in DIRECTORY"
      & header "humidor-gen - generate Haskell bindings to Qt libraries")

realMain :: Opts -> IO ()
realMain opts = do
  smokeModules <- smokeInitialize
  let conf0 = defaultGeneratorConfig (qDestDir opts) (qCabalTemplate opts)
      conf = conf0 { generatorModuleNameMap = const "Qt.Gui" }
  mapM_ (generateSmokeModule conf) smokeModules
