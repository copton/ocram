module Ocram.Test.Tests.Visitor.TranslUnit (
	tests
) where

import qualified Ocram.Test.Tests.Visitor.TranslUnit.Test1 as T1
import qualified Ocram.Test.Tests.Visitor.TranslUnit.Test2 as T2
import Ocram.Test.Tests.Visitor.Utils (runTests)

tests = runTests "TranslUnit" [T1.test, T2.test]
