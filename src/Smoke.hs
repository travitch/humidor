-- | This module provides exports for use in generated
-- bindings.
module Smoke (
  SmokeHandle,
  Index,
  smokeInvokeMethod
  ) where

import Foreign.Ptr
import Smoke.C

smokeInvokeMethod :: SmokeHandle -> Index -> Index -> Ptr () -> Ptr () -> IO ()
smokeInvokeMethod = c_smokeInvokeMethod