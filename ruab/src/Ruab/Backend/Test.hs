module Ruab.Backend.Test (tests) where

import Test.Framework (Test, testGroup)
import qualified Ruab.Backend.GDB.Test as A

tests :: Test
tests = testGroup "Backend" [A.tests]
