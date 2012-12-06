{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | A module of helpers to make using haskell-src-exts a bit simpler
module Smoke.Gen.Haskell (
  moduleName,
  nameIdent,
  tyVar,
  uktyVarBind,
  unqualIdent,
  uvar
  ) where

import qualified Data.Text as T
import Language.Haskell.Exts.Syntax

class Stringable a where
  stringify :: a -> String

instance Stringable String where
  stringify = id

instance Stringable T.Text where
  stringify = T.unpack

moduleName :: (Stringable a) => a -> ModuleName
moduleName = ModuleName . stringify

nameIdent :: (Stringable a) => a -> Name
nameIdent = Ident . stringify

unqualIdent :: (Stringable a) => a -> QName
unqualIdent = UnQual . Ident . stringify

uvar :: Name -> Exp
uvar = Var . UnQual

uktyVarBind :: String -> TyVarBind
uktyVarBind = UnkindedVar . Ident

tyVar :: String -> Type
tyVar = TyVar . Ident
