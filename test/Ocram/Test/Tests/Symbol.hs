module Ocram.Test.Tests.Symbol (
	tests
) where

import qualified Ocram.Test.Tests.Symbol.ExtDecl as A

import Test.HUnit

tests = TestLabel "Symbol" $ TestList [ A.tests ]
