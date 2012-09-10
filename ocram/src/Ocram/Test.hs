module Ocram.Test (runTests) where

import Test.Framework (defaultMainWithArgs)

import qualified Ocram.Symbols.Test as A
import qualified Ocram.Query.Test as B
import qualified Ocram.Analysis.Test as C
import qualified Ocram.Transformation.Test as D
import qualified Ocram.Debug.Test as E
import qualified Ocram.Print.Test as F
import qualified Ocram.Intermediate.Test as G

runTests :: [String] -> IO ()
runTests = defaultMainWithArgs [A.tests, B.tests, C.tests, D.tests, E.tests, F.tests, G.tests]
