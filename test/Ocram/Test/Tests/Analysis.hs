module Ocram.Test.Tests.Analysis (
	tests
) where

import qualified Ocram.Test.Tests.Analysis.TestSuites as T

import Test.HUnit

tests = TestLabel "Analysis" $ TestList T.tests
