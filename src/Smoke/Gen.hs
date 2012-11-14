-- | Generates one Haskell module per class in the Smoke module.
module Smoke.Gen (
  GeneratorConfig(..),
  defaultGeneratorConfig,
  generateSmokeModule
  ) where

import Control.Monad.Reader
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax
import System.Directory
import System.FilePath
import Text.Regex

import Smoke.C

data GeneratorConfig =
  GeneratorConfig { generatorModuleNameMap :: String -> String
                    -- ^ A function to modify the Smoke module name
                    -- (perhaps stripping off a prefix) used as the root
                    -- of all generated Haskell modules. (default: id)
                  , generatorDestDir :: FilePath
                  }

-- | A reasonable default configuration
defaultGeneratorConfig :: FilePath -> GeneratorConfig
defaultGeneratorConfig destDir =
  GeneratorConfig { generatorModuleNameMap = id
                  , generatorDestDir = destDir
                  }

type Gen = ReaderT (GeneratorConfig, String) IO

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
  return $ destDir </> map undot (modNameMap moduleName) </> typeModuleName <.> "hs"
  where
    cname = smokeClassName c
    typeModuleName = subRegex (mkRegex "::") cname "/"

classModuleName :: SmokeClass -> Gen String
classModuleName c = do
  moduleName <- asks snd
  modNameMap <- asks (generatorModuleNameMap . fst)
  return $ modNameMap moduleName <.> typeModuleName
  where
    cname = smokeClassName c
    typeModuleName = subRegex (mkRegex "::") cname "."

generateSmokeClass :: SmokeClass -> Gen ()
generateSmokeClass c = do
  fname <- locationForClass c
  lift $ createDirectoryIfMissing True (dropFileName fname)
  mname <- classModuleName c
  let loc = SrcLoc fname 0 0
      m = Module loc (ModuleName mname) [] Nothing Nothing [] []
      s = prettyPrint m
  lift $ writeFile fname s
