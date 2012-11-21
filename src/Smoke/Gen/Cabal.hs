module Smoke.Gen.Cabal ( expandCabalTemplate ) where

import Control.Monad.Trans
import Data.List ( intercalate )
import System.FilePath
import Text.StringTemplate

import Smoke.C
import Smoke.Gen.Monad
import Smoke.Gen.Util

expandCabalTemplate :: SmokeModule -> Gen ()
expandCabalTemplate m = do
  cabalTpl <- askModuleConf generatorCabalTemplate
  dest <- askModuleConf generatorDestDir
  let cs = filter (not . skipClass) (smokeModuleClasses m)
  mods <- mapM classModuleName cs
  tpl <- lift $ readFile cabalTpl
  let cabalFilename = takeFileName cabalTpl
      indent = "        "
      modSubString = intercalate ",\n" $ map (indent++) mods
      cabalFilePath = dest </> cabalFilename
      cabalContent = render $ setAttribute "modules" modSubString (newSTMP tpl)
  lift $ writeFile cabalFilePath cabalContent
