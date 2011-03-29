module Ocram.Test.Tests.Filter.Sanity (
	tests
) where

import qualified Ocram.Test.Tests.Filter.Sanity.Test1 as A
import Test.HUnit

tests = TestLabel "Sanity" $ TestList [A.tests]
