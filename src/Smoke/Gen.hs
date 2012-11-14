-- | Generates one Haskell module per class in the Smoke module.
module Smoke.Gen (
  GeneratorConfig(..),
  defaultGeneratorConfig,
  generateSmokeModule
  ) where

import Control.Monad.Reader
import System.Directory

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

generateSmokeClass :: SmokeClass -> Gen ()
generateSmokeClass = undefined
