module Smoke.Gen.SmokeImport (
  smokeImport
  ) where

import Language.Haskell.Exts.Syntax

smokeImport :: ImportDecl
smokeImport =
  ImportDecl loc (ModuleName "Smoke") False False Nothing Nothing Nothing
  where
    loc = SrcLoc "" 0 0