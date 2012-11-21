-- | This module provides exports for use in generated
-- bindings.
module Smoke (
  SmokeHandle,
  Index,
  QEnum(..),
  smokeInvokeMethod
  ) where

import Foreign.Ptr
import Smoke.C

smokeInvokeMethod :: SmokeHandle -> Index -> Index -> Ptr () -> Ptr () -> IO ()
smokeInvokeMethod = c_smokeInvokeMethod

-- | A restricted Enum that only converts its elements to Integer
-- values.  Qt enums are instances of this class.
class QEnum a where
  qenumToInt :: a -> Integer
