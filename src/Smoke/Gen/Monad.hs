{-# LANGUAGE OverloadedStrings #-}
module Smoke.Gen.Monad (
  Gen,
  GeneratorConfig(..),
  defaultGeneratorConfig,
  moduleToPath,
  askModuleName,
  askModuleConf
  ) where

import Control.Monad.Reader
import Data.Char as C
import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as T

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

askModuleConf :: (GeneratorConfig -> a) -> Gen a
askModuleConf f = asks (f . fst)

askModuleName :: Gen Text
askModuleName = asks snd

moduleToPath :: Text -> FilePath
moduleToPath = map undot . T.unpack
  where
    undot ch = if ch == '.' then '/' else ch
