module Ocram.Transformation.Test (tests) where

import Test.Framework (testGroup, Test)

import qualified Ocram.Transformation.Normalize.Test as A

tests :: Test
tests = testGroup "Transformation" [A.tests]
