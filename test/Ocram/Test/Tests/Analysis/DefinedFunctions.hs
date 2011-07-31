module Ocram.Test.Tests.Analysis.DefinedFunctions (
	tests
) where

import Ocram.Types
import Ocram.Test.Tests.Analysis.Types
import Ocram.Test.Tests.Analysis.Utils
import Ocram.Analysis.DefinedFunctions

type Input = ()
type Output = TDefinedFunctions

reduce :: Ast -> Input -> ER Output
reduce ast _ = return . reduce_df =<< defined_functions ast

setup :: TCase -> (Input, Output)
setup tc = ((), tcDefinedFunctions tc)

tests = runTests "DefinedFunctions" reduce setup
--	 ("void foo() { }", ["foo"])
--	,("", [])
--	,("int foo() {}", ["foo"])
--	,("__attribute__((bar)) int foo() {}", ["foo"])
--	,("void foo();", [])
--	,("void foo(); void bar() { }", ["bar"])
--	,("void foo() { } void bar() { }", ["bar", "foo"])
--	]
