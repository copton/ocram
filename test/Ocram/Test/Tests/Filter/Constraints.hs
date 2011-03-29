module Ocram.Test.Tests.Filter.Constraints (
	tests
) where

import qualified Ocram.Test.Tests.Filter.Constraints.FunctionPointer as A
import Test.HUnit

tests = TestLabel "Constraints" $ TestList [A.tests]
