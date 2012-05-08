module Ocram.Test.Main (main) where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import qualified Ocram.Test.Tests.Symbol as A
import qualified Ocram.Test.Tests.Query as B
import qualified Ocram.Test.Tests.Analysis as C
import qualified Ocram.Test.Tests.Transformation as D

main = defaultMain [A.tests, B.tests, C.tests, D.tests]
