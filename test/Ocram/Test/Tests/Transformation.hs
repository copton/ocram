module Ocram.Test.Tests.Transformation (
	tests
) where

import qualified Ocram.Test.Tests.Transformation.Inline as A

import Test.HUnit

tests = TestLabel "Transformation" $ TestList [ A.tests ]

