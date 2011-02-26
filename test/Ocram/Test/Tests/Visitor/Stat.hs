module Ocram.Test.Tests.Visitor.Stat (
	tests
) where

import qualified Ocram.Test.Tests.Visitor.Stat.Test1 as T1
import Ocram.Test.Tests.Visitor.Utils (runTests)

tests = runTests "Stat" T1.test
