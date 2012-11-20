-- | Produce all of the enum type definitions (and QEnum instances)
-- for the given class.
module Smoke.Gen.Enum ( makeEnumsForClass ) where

import Control.Monad ( foldM )
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as T
import Language.Haskell.Exts.Syntax

import Smoke.C
import Smoke.Gen.Monad

makeEnumsForClass :: SmokeModule -> SmokeClass -> Gen ([ExportSpec], [Decl])
makeEnumsForClass m c = do
  -- First, fold over the methods to construct a map of enum -> [value]
  let h = smokeModuleHandle m
  emap <- foldM (buildEnumMap h) mempty (smokeClassMethods c)
  foldM makeEnum ([], []) (M.toList emap)

dummyLoc :: SrcLoc
dummyLoc = SrcLoc "" 0 0

stripClassName :: String -> String
stripClassName = dropWhile (== ':') . dropWhile (/= ':')

makeEnum :: ([ExportSpec], [Decl]) -> (Text, [(Text, Integer)])
            -> Gen ([ExportSpec], [Decl])
makeEnum (sacc, dacc) (etypename, evals) = do
  -- First, the data declaration
  let dtypename = Ident $ stripClassName (T.unpack etypename)
      datDecls = map toDatDecl evals
      derivClause = [ (UnQual (Ident "Eq"), [])
                    , (UnQual (Ident "Ord"), [])
                    , (UnQual (Ident "Show"), [])
                    , (UnQual (Ident "Read"), [])
                    ]
      datDecl = DataDecl dummyLoc DataType [] dtypename [] datDecls derivClause
  -- Next, the QEnum instance
      theinst = InsDecl $ FunBind $ map toFunBind evals
      instDecl = InstDecl dummyLoc [] (UnQual (Ident "QEnum")) [TyCon (UnQual dtypename)] [theinst]
      export = EThingAll (UnQual dtypename)
  return (export : sacc, datDecl : instDecl : dacc)
  where
    toDatDecl (t, _) = QualConDecl dummyLoc [] [] (ConDecl (Ident (T.unpack t)) [])
    toFunBind (t, i) =
      let lit = PApp (UnQual (Ident (T.unpack t))) []
          rhs = UnGuardedRhs $ Lit $ Int i
      in Match dummyLoc (Ident "qenumToInt") [lit] Nothing rhs (BDecls [])

buildEnumMap :: SmokeHandle -> Map Text [(Text, Integer)] -> SmokeMethod
                -> Gen (Map Text [(Text, Integer)])
buildEnumMap h acc m
  | not (methodIsEnum m) = return acc
  | otherwise = do
    let ename = csmokeTypeName (smokeMethodRet m)
        evalName = smokeMethodName m
        cix = smokeMethodClassIndex m
        mix = smokeMethodIndex m
        eval = smokeEnumValue h cix mix
    return $ M.insertWith (++) ename [(evalName, eval)] acc
